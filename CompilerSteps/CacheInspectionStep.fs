namespace FSCL.Runtime.CacheInspection

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler
open FSCL.Runtime
open Microsoft.FSharp.Linq.RuntimeHelpers
open Cloo

[<Step("FSCL_CACHE_INSPECTION_STEP", 
       Dependencies = [| "FSCL_MODULE_PARSING_STEP" |], 
       Before = [| "FSCL_MODULE_PREPROCESSING_STEP" |])>] 
type CacheInspectionStep(tm: TypeManager,
                          processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelModule, KernelModule>(tm, processors)
    
    member this.CompareMetadata(a:KernelInfo, b:KernelInfo, requireRecompilation:HashSet<Type>) =
        false

    override this.Run(kmodule, opts) =
        if kmodule.CustomInfo.ContainsKey("RUNTIME_CACHE") then
            // Get cache
            let cache = kmodule.CustomInfo.["RUNTIME_CACHE"] :?> RuntimeCache
            // If a mathing kernel has been cached and it contains the opencl source code
            if cache.Kernels.ContainsKey(kmodule.Kernel.Info.ID) && 
               cache.Kernels.[kmodule.Kernel.Info.ID].OpenCLCode.IsSome then
                let cachedKernel = cache.Kernels.[kmodule.Kernel.Info.ID]
                kmodule.Kernel.Info.Body <- cachedKernel.Info.Body
                kmodule.Kernel.Info.Code <- cachedKernel.Info.Code
                kmodule.Kernel.Info.Name <- cachedKernel.Info.Name
                kmodule.Kernel.Info.ReturnType <- cachedKernel.Info.ReturnType
                for item in cachedKernel.Info.CustomInfo do
                    if not (kmodule.Kernel.Info.CustomInfo.ContainsKey(item.Key)) then
                        kmodule.Kernel.Info.CustomInfo.Add(item.Key, item.Value)  
                for item in cachedKernel.Info.Parameters do
                    kmodule.Kernel.Info.Parameters.Add(item)
                    (*
                // We are not going to execute further compiler steps
                // in particular the function preprocessing steps
                // Function preprocessing determines some inputs for the flow graph node,
                // for example when the kernel has a dynamic alloc array
                // We therefore need to inspect parameters and act like function preprocessing
                // creating the missing flow graph inputs
                let dynArray = Seq.tryFind(fun (p:KernelParameterInfo) -> p.IsDynamicArrayParameter) k.Info.Parameters
                // Get flow graph nodes matching the current kernel    
                let nodes = FlowGraphManager.GetKernelNodes(k.Info.ID, kmodule.FlowGraph)
                if dynArray.IsSome then                        
                    // Set flow graph argument
                    for item in nodes do
                        FlowGraphManager.SetNodeInput(item, 
                                                        dynArray.Value.Name, 
                                                        BufferAllocationSize(
                                                        fun(args, localSize, globalSize) ->
                                                            this.EvaluateReturnedBufferAllocationSize(
                                                                dynArray.Value.Type.GetElementType(), 
                                                                dynArray.Value.DynamicAllocationArguments, 
                                                                args, localSize, globalSize))) 

                // Set implicit node input for each array length arg
                for p in k.Info.Parameters do
                    if p.IsSizeParameter then
                        for item in nodes do
                            FlowGraphManager.SetNodeInput(item,
                                                            p.Name,
                                                            ImplicitValue) *)
        StopCompilation(kmodule)