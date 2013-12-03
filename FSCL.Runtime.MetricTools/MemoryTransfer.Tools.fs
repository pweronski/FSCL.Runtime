namespace FSCL.Runtime.MetricTools
    
open Cloo
open System
open Microsoft.FSharp.Reflection
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type TransferException(msg) =
    inherit System.Exception(msg)
    
type AccessException(msg) =
    inherit System.Exception(msg)

type BufferAccess =
| READ_ONLY
| WRITE_ONLY
| READ_WRITE
| NO_ACCESS

type TransferEndpoint() = 
    let isHostPtr = true
    let flags = ComputeMemoryFlags.None
    let shouldMap = false

    member val IsHostPtr = isHostPtr with get, set
    member val Flags = flags with get, set
    member val ShouldMap = shouldMap with get, set

    override this.Equals(endpoint: obj) =
        if endpoint.GetType() <> typeof<TransferEndpoint> then
            false
        else
            let endp = endpoint :?> TransferEndpoint
            (this.IsHostPtr = endp.IsHostPtr) && (this.Flags = endp.Flags) && (this.ShouldMap = endp.ShouldMap)

module MemoryUtil =
    [<DllImport("kernel32.dll")>]
    extern void RtlMoveMemory(IntPtr dest, IntPtr src, uint32 len);
    [<DllImport("msvcrl.dll")>]
    extern int memcmp(IntPtr ptr1, IntPtr ptr2, int count);
    
module TransferTools =
    let AllocateHostPtr<'T>(currSize) =
        ref (Array.zeroCreate<'T>(currSize / sizeof<'T>))
            
    let AllocateBuffer<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType>(computeContext, 
                                                                                         currSize, 
                                                                                         info: TransferEndpoint, 
                                                                                         data: 'T array) =
        if data.Length = 0 then
            ref (new ComputeBuffer<'T>(computeContext, info.Flags, [| (int64)(currSize / sizeof<'T>) |]))
        else
            ref (new ComputeBuffer<'T>(computeContext, info.Flags, data, true))

    let InitializeHostPtr<'T>(currSize, 
                              source: 'T[] ref, 
                              defValue) =
        // Init source
        Array.fill (!source) 0 ((!source).Length) defValue
        
    let InitializeBuffer<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType>(computeQueue: ComputeCommandQueue, 
                                                                                           currSize, 
                                                                                           info: TransferEndpoint, 
                                                                                           src: ComputeBuffer<'T> ref, 
                                                                                           defValue) =
        let initializer = Array.create<'T> (currSize / sizeof<'T>) defValue
        computeQueue.WriteToBuffer<'T>(initializer, !src, true, null)

    // Test copy from host ptr to host ptr
    let HostPtrToHostPtr<'T>(currSize, 
                             validate, 
                             src: 'T[] ref, 
                             dst: 'T[] ref) =         
        // From array to array, via memcpy, including allocation and initialization
        let srcHandle = GCHandle.Alloc(!src, GCHandleType.Pinned)
        let dstHandle = GCHandle.Alloc(!dst, GCHandleType.Pinned)
        let srcPtr = srcHandle.AddrOfPinnedObject()
        let dstPtr = dstHandle.AddrOfPinnedObject()
        MemoryUtil.RtlMoveMemory(dstPtr, srcPtr, (uint32)currSize)                                  
        if (srcHandle.IsAllocated) then
            srcHandle.Free()
        if (dstHandle.IsAllocated) then
            dstHandle.Free()

    // Test copy from host ptr to buffer
    let HostPtrToBuffer<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType and 'T : equality>(computeContext: ComputeContext, 
                                                                                                            computeQueue: ComputeCommandQueue, 
                                                                                                            currSize, 
                                                                                                            validate, 
                                                                                                            dstInfo: TransferEndpoint, 
                                                                                                            src: 'T[] ref, 
                                                                                                            dst: ComputeBuffer<'T> ref) =
        if dstInfo.ShouldMap then
            // From array to buffer, via map and memcpy, including allocation and initialization
            let sourceHandle = GCHandle.Alloc(!src, GCHandleType.Pinned)
            try 
                let sourcePtr = sourceHandle.AddrOfPinnedObject()
                let destPtr = computeQueue.Map<'T>(!dst, true, ComputeMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<'T>), null)
                MemoryUtil.RtlMoveMemory(destPtr, sourcePtr, (uint32)currSize)                                   
                computeQueue.Unmap(!dst, ref destPtr, null)                             
            finally
                if (sourceHandle.IsAllocated) then
                    sourceHandle.Free()
        else
            // From array to buffer, via writeBuffer, including allocation and initialization
            computeQueue.WriteToBuffer<'T>(!src, !dst, true, null)
                        
        // Validate            
        if validate then
            let finalizer = Array.zeroCreate<'T>(currSize / sizeof<'T>)
            computeQueue.ReadFromBuffer<'T>(!dst, ref finalizer, true, null)
            Array.iteri (fun i element -> 
                if element <> finalizer.[i] then
                    raise (new TransferException("Source and destination do not match"))) !src   

    // Test copy from buffer to host ptr
    let BufferToHostPtr<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType and 'T: equality>(computeContext: ComputeContext, 
                                                                                                           computeQueue: ComputeCommandQueue, 
                                                                                                           currSize, 
                                                                                                           validate, 
                                                                                                           srcInfo: TransferEndpoint, 
                                                                                                           src: ComputeBuffer<'T> ref, 
                                                                                                           dst: 'T[] ref,
                                                                                                           defValue : 'T) =
        if srcInfo.ShouldMap then
            // From buffer to array, via map and memcpy, including allocation
            let destHandle = GCHandle.Alloc(!dst, GCHandleType.Pinned)
            try 
                let destPtr = destHandle.AddrOfPinnedObject()
                let sourcePtr = computeQueue.Map<'T>(!src, true, ComputeMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<'T>), null)
                MemoryUtil.RtlMoveMemory(destPtr, sourcePtr, (uint32)currSize) 
                computeQueue.Unmap(!src, ref sourcePtr, null)                                                                 
            finally
                if (destHandle.IsAllocated) then
                    destHandle.Free() 
        else
             computeQueue.ReadFromBuffer<'T>(!src, dst, true, null)

        // Validate
        if validate then
            Array.iteri (fun i element -> 
                if element <> defValue then
                    raise (new TransferException("Source and destination do not match"))) !dst                                   
                
    // Test copy from buffer to buffer
    let BufferToBuffer<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType and 'T: equality>(computeContext: ComputeContext, 
                                                                                                          computeQueue: ComputeCommandQueue, 
                                                                                                          currSize, 
                                                                                                          validate, 
                                                                                                          srcInfo: TransferEndpoint, 
                                                                                                          dstInfo: TransferEndpoint, 
                                                                                                          src: ComputeBuffer<'T> ref, 
                                                                                                          dst: ComputeBuffer<'T> ref,
                                                                                                          defValue : 'T) =
        if srcInfo.ShouldMap && dstInfo.ShouldMap then
            // From buffer to buffer, via map and memcpy, including allocation
            let destPtr = computeQueue.Map<'T>(!dst, true, ComputeMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<'T>), null)
            let sourcePtr = computeQueue.Map<'T>(!src, true, ComputeMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<'T>), null)
            MemoryUtil.RtlMoveMemory(destPtr, sourcePtr, (uint32)currSize) 
            computeQueue.Unmap(!dst, ref destPtr, null)            
            computeQueue.Unmap(!src, ref sourcePtr, null)    
        elif srcInfo.ShouldMap && (not dstInfo.ShouldMap) then
            // From buffer to buffer, via writeBuffer, including allocation
            let sourcePtr = computeQueue.Map<'T>(!src, true, ComputeMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<'T>), null)
            computeQueue.Write<'T>(!dst, true, 0L, (int64)(currSize / sizeof<'T>), sourcePtr, null)
            computeQueue.Unmap(!src, ref sourcePtr, null)  
        elif (not srcInfo.ShouldMap) && dstInfo.ShouldMap then
            // From buffer to buffer, via readBuffer, including allocation
            let destPtr = computeQueue.Map<'T>(!dst, true, ComputeMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<'T>), null)
            computeQueue.Read<'T>(!src, true, 0L, (int64)(currSize / sizeof<'T>), destPtr, null)
            computeQueue.Unmap(!dst, ref destPtr, null)
        else                                                    
            // From buffer to buffer, via buffer copy, including allocation
            computeQueue.CopyBuffer<'T>(!src, !dst, null)  
            computeQueue.Finish()
            
        // Validate
        if validate then
            let finalizer = Array.zeroCreate<'T>(currSize / sizeof<'T>)
            computeQueue.ReadFromBuffer<'T>(!dst, ref finalizer, true, null)
            Array.iteri (fun i element -> 
                if element <> defValue then
                    raise (new TransferException("Source and destination do not match"))) finalizer       
 