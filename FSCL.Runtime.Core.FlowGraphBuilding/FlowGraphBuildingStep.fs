namespace FSCL.Runtime.FlowGraphBuilding

open System
open FSCL.Compiler
open FSCL.Runtime
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

[<Step("FSCL_RUNTIME_FLOW_GRAPH_STEP")>]
type FlowGraphBuildingStep(tm: TypeManager,
                           processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<Expr * Compiler * RuntimeCache, FlowGraphNode>(tm, processors)

    let mutable opts = null
    let mutable compiler = null
    let mutable rcache = null

    member this.Compiler
        with get() =
            compiler
            
    member this.Cache
        with get() =
            rcache
            
    member this.Process(expr: Expr) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < processors.Length) do
            output <- processors.[index].Execute(expr, this, opts) :?> FlowGraphNode option
            index <- index + 1
        if output.IsNone then
            raise (KernelExecutionException("The runtime is not able to build flow graph node for expression " + expr.ToString()))
        output.Value

    override this.Run((expr, comp, cache), opt) =
        opts <- opt
        compiler <- comp
        rcache <- cache
        ValidResult(this.Process(expr))