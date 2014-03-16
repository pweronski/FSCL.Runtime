namespace FSCL.Runtime.FlowGraphBuilding

open System
open FSCL.Compiler
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_REFLECTED_CALL_FLOW_GRAPH_PROCESSOR", "FSCL_RUNTIME_FLOW_GRAPH_STEP")>]
type ReflectedCallFlowGraphProcessor() =      
    inherit CompilerStepProcessor<Expr, FlowGraphNode option>()
    
    member private this.LiftArgumentsAndKernelCalls(e: Expr,
                                                    args: Dictionary<string, obj>,
                                                    localSize: int64 array,
                                                    globalSize: int64 array) =
        match e with
        // Return allocation expression can contain a call to global_size, local_size, num_groups or work_dim
        | Patterns.Call(o, m, arguments) ->
            if m.DeclaringType.Name = "Language" && (m.Name = "get_global_size") then
                Expr.Value(globalSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int])
            else if m.DeclaringType.Name = "Language" && (m.Name = "get_local_size") then
                Expr.Value(localSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int])
            else if m.DeclaringType.Name = "Language" && (m.Name = "get_num_groups") then
                let gs = globalSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int]
                let ls = localSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int]
                Expr.Value(int (Math.Ceiling(float gs / float ls)))
            else if m.DeclaringType.Name = "Language" && (m.Name = "get_work_dim") then
                Expr.Value(globalSize.Rank)
            else
                if o.IsSome then
                    let evaluatedIstance = this.LiftArgumentsAndKernelCalls(o.Value, args, localSize, globalSize);
                    let liftedArgs = List.map(fun (e: Expr) -> this.LiftArgumentsAndKernelCalls(e, args, localSize, globalSize)) arguments;
                    Expr.Call(
                        evaluatedIstance,
                        m, 
                        liftedArgs)
                else
                    Expr.Call(
                        m, List.map(fun (e: Expr) -> this.LiftArgumentsAndKernelCalls(e, args, localSize, globalSize)) arguments)
        // Return allocation expression can contain references to arguments
        | Patterns.Var(v) ->
            if (args.ContainsKey(v.Name)) then
                let t = args.[v.Name].GetType()
                Expr.Value(args.[v.Name], t)
            else
                e                
        | ExprShape.ShapeVar(v) ->
            e
        | ExprShape.ShapeLambda(l, b) ->
            failwith "Error in substituting parameters"
        | ExprShape.ShapeCombination(c, argsList) ->
            ExprShape.RebuildShapeCombination(c, List.map(fun (e: Expr) -> this.LiftArgumentsAndKernelCalls(e, args, localSize, globalSize)) argsList)

    member private this.EvaluateBufferAllocationSize(t: Type,
                                                     sizes: Expr list,
                                                     args: Dictionary<string, obj>, 
                                                     localSize: int64 array,
                                                     globalSize: int64 array) =   
        let intSizes = new List<int64>()    
        for exp in sizes do
            let lifted = this.LiftArgumentsAndKernelCalls(exp, args, localSize, globalSize)
            let evaluated = LeafExpressionConverter.EvaluateQuotation(lifted)
            intSizes.Add((evaluated :?> int32) |> int64)
        ExplicitAllocationSize(intSizes |> Seq.toArray)       

    override this.Run(expr, s, opts) =
        let step = s :?> FlowGraphBuildingStep
        let compiler = step.Compiler
        let cache = step.Cache

        // Check if expr is a call to a reflected definition method
        match expr with
        | Patterns.Call(o, mi, args) ->
            match mi with
            | DerivedPatterns.MethodWithReflectedDefinition(body) ->
                let kmod, src = compiler.Compile((expr, cache), opts) :?> (KernelModule * string) 
                                                
                // Check params to build node input
                let nodeInput = Array.create (kmod.Kernel.Info.Parameters.Count) ImplicitValue
                for i = 0 to kmod.Kernel.Info.Parameters.Count - 1 do
                    let p = kmod.Kernel.Info.Parameters.[i]
                    if p.Type.IsArray then
                        // Check it's an input kernel
                        if p.CallExpr.IsSome then
                            try
                                let inputKernel = step.Process(p.CallExpr.Value)
                                nodeInput.[i] <- KernelOutput(inputKernel, 0)
                            with 
                                | :? CompilerException ->
                                    // Dynamically allocated buffer
                                    if p.IsDynamicArrayParameter then
                                        nodeInput.[i] <- BufferAllocationSize(
                                                            fun(args, localSize, globalSize) ->
                                                                this.EvaluateBufferAllocationSize(p.Type.GetElementType(), p.DynamicAllocationArguments, args, localSize, globalSize))
                                    else 
                                        // Buffer from actual array
                                        nodeInput.[i] <- ActualArgument(kmod.Kernel.Info.Parameters.[i].CallExpr.Value)
                    else
                        // Value
                        if not p.IsSizeParameter then
                            nodeInput.[i] <- ActualArgument(kmod.Kernel.Info.Parameters.[i].CallExpr.Value)
                None
            | _ ->
                None
        | _ ->
            None