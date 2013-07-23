namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Compiler
open System.Collections.Generic
open System
open System.Threading

module KernelRunner =
    // The Kernel runner
    type internal Runner(compiler, configuration, metric) =    
        
        member val KernelManager = new KernelManager(compiler, metric) with get
        member val KernelDiscovery = new KernelDiscoveryStep(RuntimeBuilder.Build(configuration)) with get
        
        member this.RunOpenCL(graph:RuntimeCallGraph,
                              globalSize:int array, 
                              localSize:int array) =

            // The storage for buffers potentially used by successive kernels in the call graph flow
            let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory>>()

            // Foreach kernel in the dependency order
            for kernel in graph.Kernels do
                // Get runtime cache data
                match kernel.RuntimeCacheData with
                | Some(device, kernelInfo, compiledKernel) ->
                    let mutable argIndex = 0
                    // Foreach argument of the kernel
                    for par in  kernelInfo.Info.ParameterInfo do
                        // Get the relative binding (if exists)
                        if kernel.ArgumentsInputBinding.ContainsKey(par.Key) then                            
                            // If the argument is bound to an expression
                            let argBinding = kernel.ArgumentsInputBinding.[par.Key]
                            match argBinding with
                            | InputExpressionBinding(e) ->
                                // Evaluate expression
                                let o = e.EvalUntyped()
                                // If the parameter requires a buffer
                                if par.Value.Info.ParameterType.IsArray then
                                    // Check if read or read_write modeS
                                    let matchingParameter = kernelInfo.Info.ParameterInfo.[par.Value.Info.Name]
                                    let access = matchingParameter.Access
                                    let mustInitBuffer =
                                        ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                                            (matchingParameter.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                                        ((access = KernelParameterAccessMode.ReadOnly) || 
                                            (access = KernelParameterAccessMode.ReadWrite))                                    
                                    // Create buffer and eventually init it, and set kernel arg
                                    let t = par.Value.Info.ParameterType.GetElementType()
                                    let buffer = BufferTools.WriteBuffer(t, device.Context, device.Queue, o, par.Value.SizeParameters.Length, mustInitBuffer)   
                                    compiledKernel.Kernel.SetMemoryArgument(argIndex, buffer.Value)                                        
                                    // Store association between parameter, array and buffer object
                                    // (only if the returnBinding is OutputKernel and the return parameter is 
                                    // the one associated to this buffer, that is this buffer will be used by another kernel later)
                                    match kernel.ReturnBinding with
                                    | OutputKernelBinding(mi, pName) ->
                                        if par.Value.IsReturnParameter then
                                            if not (bufferBinding.ContainsKey(kernel.ID)) then
                                                bufferBinding.Add(kernel.ID, new Dictionary<String, ComputeMemory>())
                                            bufferBinding.[kernel.ID].Add(par.Key, buffer.Value)
                                    | _ ->
                                        ()
                                else
                                    compiledKernel.Kernel.SetValueArgumentAsObject(argIndex, o)
                                    
                            | InputKernelBinding(mi, pName) ->
                                let inputBuffer = bufferBinding.[mi].[pName]
             //-->              // We should AVOID COPY
                                // Check if read or read_write mode
                                let matchingParameter = kernelInfo.Info.ParameterInfo.[par.Value.Info.Name]
                                let access = matchingParameter.Access                               
                                // Create buffer and eventually init it, and set kernel arg
                                let t = par.Value.Info.ParameterType.GetElementType()
                                let buffer = BufferTools.CopyBuffer(t, device.Context, device.Queue, inputBuffer)   
                                compiledKernel.Kernel.SetMemoryArgument(argIndex, buffer.Value)                                        
                                // Store association between parameter, array and buffer object
                                // (only if the returnBinding is OutputKernel and the return parameter is 
                                // the one associated to this buffer, that is this buffer will be used by another kernel later)
                                match kernel.ReturnBinding with
                                | OutputKernelBinding(mi, pName) ->
                                    if par.Value.IsReturnParameter then
                                        if not (bufferBinding.ContainsKey(kernel.ID)) then
                                            bufferBinding.Add(kernel.ID, new Dictionary<String, ComputeMemory>())
                                        bufferBinding.[kernel.ID].Add(par.Key, buffer.Value)
                                | _ ->
                                    ()

                            | NoInputBinding ->                                
                                // If the parameter requires a buffer (should be local)
                                if par.Value.Info.ParameterType.IsArray then
                                    // Check if local buffer. In this case we pass the dimension (sizeof) the array and not a real buffer
                                    if kernelInfo.Info.ParameterInfo.[par.Value.Info.Name].AddressSpace = KernelParameterAddressSpace.LocalSpace then
                                        let size = (o.GetType().GetProperty("LongLength").GetValue(o) :?> int64) * 
                                                    (int64 (System.Runtime.InteropServices.Marshal.SizeOf(o.GetType().GetElementType())))
                                        // Set kernel arg
                                        kernelInstance.Kernel.SetLocalArgument(!argIndex, size) 
                                // If not (should be a size parameter)
                                let sizeOfDim = o.GetType().GetMethod("GetLength").Invoke(o, [| dimension |]) :?> int
                                kernelInstance.Kernel.SetValueArgument<int>(argumentsInfo.Length + !additionalArgCount + dimension, sizeOfDim)
                        // Next arg
                        argIndex <- argIndex + 1

            // Run kernel
            let offset = Array.zeroCreate<int64>(globalSize.Length)
            // 32 bit enought for size_t. Kernel uses size_t like int withour cast. We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
            queue.Execute(kernelInstance.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)

            // Read result if needed
            Array.iteri (fun index (par:ParameterInfo, dim:int, arg:Expr) ->
                if par.ParameterType.IsArray then
                    if kernel.Info.ParameterInfo.[par.Name].AddressSpace <> KernelParameterAddressSpace.LocalSpace then
                        // Get association between parameter, array and buffer object
                        let (o, buffer) = paramObjectBufferMap.[par.Name]

                        // Check if write or read_write mode
                        let mutable mustReadBuffer = false
                        let matchingParameter = kernel.Info.ParameterInfo.[par.Name]
                        let access = matchingParameter.Access
                        mustReadBuffer <-                     
                            ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace)) &&
                            ((access = KernelParameterAccessMode.WriteOnly) || 
                                (access = KernelParameterAccessMode.ReadWrite))

                        if(mustReadBuffer) then
                            // Create buffer and eventually init it
                            let t = par.ParameterType.GetElementType()                          
                            BufferTools.ReadBuffer(t, context, queue, o, dim, buffer)) argumentsInfo 

        member this.RunMultithread(kernel: FSCLKernelData, argumentsInfo: (ParameterInfo * int * Expr)[], globalSize: int array, localSize: int array, multithread: bool) =
            let globalDataStorage = this.KernelManager.GlobalDataStorage

            let arguments = Array.map (fun (p, d, e:Expr) -> e.EvalUntyped()) argumentsInfo 
            // Normalize dimensions of workspace
            // If the workspace is one dim, treansform into 3 dims with the second and the third equals to 1 (thread)
            let normalizedGlobalSize, normalizedLocalSize = 
                match globalSize.Rank with
                | 1 ->
                    ([| globalSize.[0]; 1; 1 |], [| localSize.[0]; 1; 1 |])
                | 2 ->
                    ([| globalSize.[0]; globalSize.[1]; 1 |], [| localSize.[0]; localSize.[1]; 1 |])
                | _ ->
                    (globalSize, localSize)

            // Launch threads or execute sequential
            let work = kernel.Info.Source
            for i = 0 to normalizedGlobalSize.[0] - 1 do
                for j = 0 to normalizedGlobalSize.[1] - 1 do
                    for k = 0 to normalizedGlobalSize.[2] - 1 do
                        // Create a ids container for each thread and run the thread
                        let container = new WorkItemIdContainer(globalSize, 
                                                                localSize, 
                                                                [| i; j; k |], 
                                                                [| i / normalizedGlobalSize.[0]; j / normalizedGlobalSize.[1]; k / normalizedGlobalSize.[2] |],
                                                                [| 0; 0; 0 |])
                        if multithread then
                            // Create thread
                            let t = new Thread(new ThreadStart(fun () -> work.Invoke(null, Array.append arguments [| container |]) |> ignore))
                            t.Start()
                        else
                            work.Invoke(null, Array.append arguments [| container |]) |> ignore
        
        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, 
                        globalSize: int array, 
                        localSize: int array, 
                        mode: KernelRunningMode, 
                        fallback: bool) =                     
            // Execute kernel discovery to get the call graph
            let runtimeCallGraph = this.KernelDiscovery.Run(expr)
            this.KernelManager.CompileAndCache(runtimeCallGraph, mode, fallback)

            match mode with
            | KernelRunningMode.OpenCL ->
                this.RunOpenCL(runtimeCallGraph, globalSize, localSize)
            | KernelRunningMode.Multithread ->
                this.RunMultithread(runtimeCallGraph, globalSize, localSize, true)
            | _ ->
                this.RunMultithread(runtimeCallGraph, globalSize, localSize, false)
          
    
    // Global kernel runner
    let internal kernelRunner = new Runner(new Compiler(), RuntimeConfigurationManager.LoadConfiguration(), None)

    // Function to set custom kernel manager
    let Init(compiler, metric) =
        kernelRunner = new Runner(compiler, RuntimeConfigurationManager.LoadConfiguration(), metric)
        
    let Init(compiler, conf, metric) =
        kernelRunner = new Runner(compiler, conf, metric)

    // List available devices
    let ListDevices() = 
        List.ofSeq(seq {
                        for platform in Cloo.ComputePlatform.Platforms do
                            yield List.ofSeq(seq {
                                                    for device in platform.Devices do
                                                        yield (device.VendorId, device.Name)
                                             })
                   })

    // Extension methods to run a quoted kernel
    type Expr with
        member this.Run(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, true)
        member this.Run(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, true)
            
        member this.RunOpenCL(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, false)
        member this.RunOpenCL(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, false)
            
        member this.RunMultithread(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.Multithread, true)
        member this.RunMultithread(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.Multithread, true)
            
        member this.RunSequential(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.Sequential, true)
        member this.RunSequential(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.Sequential, true)
            
            

