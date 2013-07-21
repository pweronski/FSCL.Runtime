namespace FSCL.Runtime
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection
open Microsoft.FSharp.Quotations
open System
open FSCL.Compiler

type ArgumentBindingMode =
| NoBinding
| InputExpression of Expr
| OutputExpression
| InputKernel of MethodInfo * int
| OutputKernel of MethodInfo * int

[<AllowNullLiteral>]
type CallGraphNode(content: KernelInfo) =
    member val Content = content with get
    member val ArgumentsBinding = new List<ArgumentBindingMode>() with get
    member val ReturnBinding = OutputExpression with get, set
    member this.IsEndPoint
        with get() =
            let pOut = (Seq.tryFind(fun (b:ArgumentBindingMode) -> 
                            match b with 
                            | OutputKernel(k, i) ->
                                true
                            | _ ->
                                false) (this.ArgumentsBinding)).IsNone
            let rOut = match this.ReturnBinding with
                       | OutputKernel(k, i) ->
                            true
                       | _ ->
                            false
            pOut || rOut
    member this.IsEntryPoint
        with get() =
            (Seq.tryFind(fun (b:ArgumentBindingMode) -> 
                            match b with 
                            | InputKernel(k, i) ->
                                true
                            | _ ->
                                false) (this.ArgumentsBinding)).IsNone

[<AllowNullLiteral>]
type RuntimeCallGraph() =
    member val internal kernelStorage = new Dictionary<MethodInfo, CallGraphNode>() 

    member this.HasKernel(info: MethodInfo) =
        this.kernelStorage.ContainsKey(info)
            
    member this.GetKernel(info: MethodInfo) =
        if this.kernelStorage.ContainsKey(info) then
            this.kernelStorage.[info].Content
        else
            null
            
    member internal this.GetCallgraphNode(info: MethodInfo) =
        if this.kernelStorage.ContainsKey(info) then
            this.kernelStorage.[info]
        else
            null

    member this.AddKernel(info: KernelInfo) =
        if not (this.kernelStorage.ContainsKey(info.ID)) then
            this.kernelStorage.Add(info.ID, new CallGraphNode(info))
            
    member this.RemoveKernel(info: MethodInfo) =
        if this.kernelStorage.ContainsKey(info) then
            // Remove connections
            for kernel in this.kernelStorage do
                for i = 0 to kernel.Value.ArgumentsBinding.Count - 1 do
                    match kernel.Value.ArgumentsBinding.[i] with
                    | OutputKernel(m, i) ->
                        if m = info then
                            kernel.Value.ArgumentsBinding.[i] <- NoBinding
                    | _ ->
                        ()      
            for i = 0 to this.kernelStorage.[info].ArgumentsBinding.Count - 1 do
                match this.kernelStorage.[info].ArgumentsBinding.[i] with
                | InputKernel(m, j) ->
                    if m = info then
                        this.kernelStorage.[m].ArgumentsBinding.[j] <- NoBinding
                | _ ->
                    ()                                  
            // Remove the item
            this.kernelStorage.Remove(info) |> ignore

    member this.MergeWith(cg:RuntimeCallGraph) =
        for k in cg.KernelIDs do
            let cgn = cg.GetCallgraphNode(k)
            this.AddKernel(cgn.Content)
            for p = 0 to cgn.ArgumentsBinding.Count - 1 do
                this.SetBinding(k, p, cgn.ArgumentsBinding.[p])
            this.SetBinding(k, cgn.ReturnBinding)
            
    member this.SetBinding(src: MethodInfo, p:int, binding:ArgumentBindingMode) =
        if this.kernelStorage.ContainsKey(src) then
            if not (this.kernelStorage.[src].ArgumentsBinding.Count <= p) then
                this.kernelStorage.[src].ArgumentsBinding.Add(binding) 
            else
                this.kernelStorage.[src].ArgumentsBinding.[p] <- binding
                
    member this.SetBinding(src: MethodInfo, binding:ArgumentBindingMode) =
        if this.kernelStorage.ContainsKey(src) then
                this.kernelStorage.[src].ReturnBinding <- binding

    member this.EntyPoints = 
        List.ofSeq(
            Seq.map(fun (g:CallGraphNode) ->
                        g.Content.ID) ((Seq.filter(fun (k: CallGraphNode) -> k.IsEntryPoint)) (this.kernelStorage.Values)))
        
    member this.EndPoints =  
        List.ofSeq(
            Seq.map(fun (g:CallGraphNode) ->
                        g.Content.ID) ((Seq.filter(fun (k: CallGraphNode) -> k.IsEndPoint)) (this.kernelStorage.Values)))
      
    // Return in a breath-first mode
    member this.KernelIDs =
        let l = new List<MethodInfo>()
        l.AddRange(this.EntyPoints)
        for i = 0 to l.Count - 1 do
            for ab in this.kernelStorage.[l.[i]].ArgumentsBinding do
                match ab with
                | OutputKernel(m, j) ->
                    l.Add(m)
                | _ ->
                    ()
        List.ofSeq(l)
        
        
