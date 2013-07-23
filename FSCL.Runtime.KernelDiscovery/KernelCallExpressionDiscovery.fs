namespace FSCL.Runtime.KernelDiscovery

open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

[<KernelDiscoveryProcessor("FSCL_CALL_EXPRESSION_DISCOVERY_PROCESSOR")>]
type KernelCallExpressionDiscovery() =   
    let rec LiftArgExtraction(expr) =
        match expr with
        | Patterns.Let(v, value, body) ->
            match value with
            | Patterns.TupleGet(te, i) ->
                LiftArgExtraction(body)
            | _ ->
                (expr)
        | _ ->
            (expr)
             
    interface IKernelDiscoveryProcessor with
        member this.Run(obj, step) =
            if (obj :? Expr) then
                match obj :?> Expr with
                // Case k2(k1(args), ...) where k1 doesn't return a tuple value
                | Patterns.Call(o, mi, args) ->
                    let cg = new RuntimeCallGraph()
                    let arguments = new List<Object>()
                    // Extract sub kernels and merge them with the current call graph
                    List.iter(fun (e: Expr) -> 
                        try 
                            let sk = step.Run(e)                            
                            arguments.Add(sk.EndPoints.[0].ID)
                            cg.MergeWith(sk)
                        with
                            | :? KernelDiscoveryException -> 
                                arguments.Add(e)) args
                    // Add current kernel to the call graph
                    try 
                        let current = step.Run(mi)
                        cg.MergeWith(current)
                        // Add connections
                        for argIndex = 0 to arguments.Count - 1 do
                            if arguments.[argIndex] :? MethodInfo then
                                cg.SetInputBinding(current.Kernels.[0].ID, mi.GetParameters().[argIndex].Name, 
                                                   InputKernelBinding(arguments.[argIndex] :?> MethodInfo, "0"))
                                cg.SetReturnBinding(arguments.[argIndex] :?> MethodInfo, OutputKernelBinding(current.Kernels.[0].ID, [ mi.GetParameters().[argIndex].Name ]))
                    with
                        | :? KernelDiscoveryException -> 
                            raise (KernelCompilationException("Invalid kernel expression: call to " + mi.Name))
                    Some(cg)
                | Patterns.Let(v, value, body) ->
                    (* 
                     * Check if we have something like:
                     * Let(tupleArg, CALLTOSUBKERNEL, Let(a..., Let(b..., CALLTOKERNEL)))
                     * This means we are returning a tuple value from the subkernel and using it
                     * to assign multiple arguments of the outer kernel
                     * This seems to happen only if KERNEL is f(a,b..z) and SUBKERNEL returns (a,b...z)
                     * (i.e. the subkernel "fills" all the parameters of kernel)
                     * but not otherwise (e.g. kernel is f(a,b,...z) and subkernel returns (a,b...x < z)
                     *)
                    if v.Name = "tupledArg" then
                        let lifted = LiftArgExtraction(body)
                        match lifted with
                        | Patterns.Call(o, mi, args) ->     
                            let cg = new RuntimeCallGraph()
                            let argument = ref (new Object())
                            // Extract sub kernels and merge them with the current call graph (one only argument if arrived here)
                            try 
                                let sk = step.Run(value)                            
                                argument := sk.EndPoints.[0].ID :> obj
                                cg.MergeWith(sk)
                            with
                                | :? KernelDiscoveryException -> 
                                    argument := args.[0] :> obj
                            // Add current kernel to the call graph
                            try 
                                let current = step.Run(mi)
                                cg.MergeWith(current)
                                // Add connections
                                let retTypes =
                                    if FSharpType.IsTuple((!argument :?> MethodInfo).ReturnType) then
                                        FSharpType.GetTupleElements((!argument :?> MethodInfo).ReturnType)
                                    else
                                        [| (!argument :?> MethodInfo).ReturnType |]
                                for retTypeIndex = 0 to retTypes.Length - 1 do                                    
                                    cg.SetInputBinding(current.Kernels.[0].ID, mi.GetParameters().[retTypeIndex].Name, 
                                                        InputKernelBinding(!argument :?> MethodInfo, retTypeIndex.ToString()))
                                cg.SetReturnBinding(!argument:?> MethodInfo, 
                                                    OutputKernelBinding(
                                                        current.Kernels.[0].ID,
                                                        List.ofSeq(seq {
                                                            for i = 0 to retTypes.Length - 1 do
                                                                yield mi.GetParameters().[i].Name
                                                        })))
                                // Return
                                Some(cg)                                
                            with
                                | :? KernelDiscoveryException -> 
                                    raise (KernelCompilationException("Invalid kernel expression: call to " + mi.Name))
                        | _ ->
                            None
                    else
                        None
                | _ ->
                    None
            else
                None
            