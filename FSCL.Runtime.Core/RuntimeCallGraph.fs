namespace FSCL.Runtime
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection
open Microsoft.FSharp.Quotations
open System
open FSCL.Compiler

type ArgumentInputBindingMode =
| NoInputBinding
| InputExpressionBinding of Expr
| InputKernelBinding of MethodInfo * string

type ArgumentOutputBindingMode =
| NoOutputBinding
| OutputKernelBinding of MethodInfo * (string list)

[<AllowNullLiteral>]
type CallGraphNode(id:MethodInfo, 
                   ?parsingExpr: Expr) =
    member val RuntimeCacheData:(RuntimeDeviceData * RuntimeKernelData * RuntimeCompiledKernelData) option = None with get, set
    member val ID = id with get
    member val Device:DeviceAttribute = null with get, set
    member val ParsingExpression = parsingExpr with get
    member val ArgumentsInputBinding = new Dictionary<String, ArgumentInputBindingMode>() with get
    //member val ArgumentsOutputBinding = Array.create<ArgumentOutputBindingMode> (id.GetParameters().Length) NoOutputBinding with get
    member val ReturnBinding = NoOutputBinding with get, set
    member this.IsEndPoint
        with get() =
            (*let pOut = (Seq.tryFind(fun (b:ArgumentOutputBindingMode) -> 
                            match b with 
                            | OutputKernelBinding(k, i) ->
                                true
                            | _ ->
                                false) (this.ArgumentsOutputBinding)).IsNone *)
            match this.ReturnBinding with
                | OutputKernelBinding(k, l) ->
                    false
                | _ ->
                    true
                //pOut || rOut
    member this.IsEntryPoint
        with get() =
            (Seq.tryFind(fun (b:ArgumentInputBindingMode) -> 
                            match b with 
                            | InputKernelBinding(k, s) ->
                                true
                            | _ ->
                                false) (this.ArgumentsInputBinding.Values)).IsNone

[<AllowNullLiteral>]
type RuntimeCallGraph() =
    member val internal kernelStorage = new Dictionary<MethodInfo, CallGraphNode>() 

    member this.Has(info: MethodInfo) =
        this.kernelStorage.ContainsKey(info)
            
    member this.Get(info: MethodInfo) =
        if this.kernelStorage.ContainsKey(info) then
            this.kernelStorage.[info]
        else
            null

    member this.Add(id: MethodInfo, ?parsingExpr: Expr) =
        if not (this.kernelStorage.ContainsKey(id)) then
            if parsingExpr.IsSome then
                this.kernelStorage.Add(id, new CallGraphNode(id, parsingExpr.Value))
            else
                this.kernelStorage.Add(id, new CallGraphNode(id))
            
    member this.MergeWith(cg: RuntimeCallGraph) =
        for kernel in cg.Kernels do
            this.kernelStorage.Add((kernel:CallGraphNode).ID, kernel)
            for p in kernel.ArgumentsInputBinding.Keys do
                this.SetInputBinding((kernel:CallGraphNode).ID, p, kernel.ArgumentsInputBinding.[p])
            //for p = 0 to kernel.ArgumentsOutputBinding.Length - 1 do
              //  this.SetOutputBinding((kernel:CallGraphNode).ID, p, kernel.ArgumentsOutputBinding.[p])
            this.SetReturnBinding((kernel:CallGraphNode).ID, kernel.ReturnBinding)
            
    member this.SetInputBinding(src: MethodInfo, p: string, binding: ArgumentInputBindingMode) =
        if this.kernelStorage.ContainsKey(src) then
            if this.kernelStorage.[src].ArgumentsInputBinding.ContainsKey(p) then
                this.kernelStorage.[src].ArgumentsInputBinding.[p] <- binding
            else
                this.kernelStorage.[src].ArgumentsInputBinding.Add(p, binding)
                
    //member this.SetOutputBinding(src: MethodInfo, p: int, binding: ArgumentOutputBindingMode) =
      //  if this.kernelStorage.ContainsKey(src) then
        //    this.kernelStorage.[src].ArgumentsOutputBinding.[p] <- binding
            
    member this.SetReturnBinding(src: MethodInfo, binding: ArgumentOutputBindingMode) =
        if this.kernelStorage.ContainsKey(src) then
            this.kernelStorage.[src].ReturnBinding <- binding

    member this.EntyPoints = 
        List.ofSeq((Seq.filter(fun (k: CallGraphNode) -> k.IsEntryPoint) (this.kernelStorage.Values)))
        
    member this.EndPoints =  
        List.ofSeq((Seq.filter(fun (k: CallGraphNode) -> k.IsEndPoint) (this.kernelStorage.Values)))
      
    // Return in a breath-first mode
    member this.Kernels =
        let l = new List<CallGraphNode>()
        l.AddRange(this.EntyPoints)
        for i = 0 to l.Count - 1 do
            //for ab in this.kernelStorage.[l.[i].ID].ArgumentsOutputBinding do
                match this.kernelStorage.[l.[i].ID].ReturnBinding with
                | OutputKernelBinding(m, sl) ->
                    l.Add(this.kernelStorage.[m])
                | _ ->
                    ()
        List.ofSeq(l)
        
        
