namespace FSCL.Runtime
open System
open System.Collections.Generic
open FSCL.Compiler
open FSCL.Compiler.Language

// Kernel run mode
type KernelRunningMode =
| OpenCL
| Multithread
| Sequential

module HostLanguage =
    type WorkSizeAttribute(globalSize:int64 array, localSize:int64 array) =
        inherit DynamicKernelMetadataAttribute()

        new(globalSize: int64, localSize: int64) =
            new WorkSizeAttribute([| globalSize |], [| localSize |])

        member val GlobalSize = globalSize with get
        member val LocalSize = localSize with get
        
    [<DynamicMetadataFunction(typeof<WorkSizeAttribute>)>]
    let WORKSIZE(globalSize:int64 array, localSize:int64 array, comp) =
        comp
        
        
        


