namespace FSCL.Runtime

open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection
open Microsoft.FSharp.Quotations
open System
open System.Collections.ObjectModel
open System.Collections.Generic
open FSCL.Compiler

type BufferAllocationSizeExpression =
| ExplicitAllocationSize of int64 array
| BufferReferenceAllocationExpression of string

type FlowGraphNodeInput =
| KernelOutput of FlowGraphNode * int
| ActualArgument of Expr
| BufferAllocationSize of (Dictionary<string, obj> * int64 array * int64 array -> BufferAllocationSizeExpression)
| CompilerPrecomputedValue of (Dictionary<string, obj> * int64 array * int64 array -> obj)
| ImplicitValue

and FlowGraphNodeInputInfo(i: FlowGraphNodeInput) =

    member val Input = i with get

and [<AllowNullLiteral>] FlowGraphNode(kernel: KernelInfo, 
                                       mi: MethodInfo option) =            
    member val Kernel = kernel with get
    member val internal Input = new Dictionary<string, FlowGraphNodeInputInfo>() with get
    member val internal Output = (null, new Dictionary<int, string>()) with get, set
  
    member val CustomInfo = new Dictionary<String, Object>() with get
    
    member this.IsEntryPoint 
        with get() =
            (Seq.tryFind(
                fun(k: KeyValuePair<string, FlowGraphNodeInputInfo>) ->
                    match k.Value.Input with 
                    | KernelOutput(n, i) ->
                        true
                    | _ ->
                        false
                ) (this.Input)).IsNone
            
    member this.IsEndPoint 
        with get() =
            fst(this.Output) = null

[<AllowNullLiteral>]
type FlowGraphManager() =
    static member GetNodeInput(n: FlowGraphNode) =
        new ReadOnlyDictionary<string, FlowGraphNodeInputInfo>(n.Input)

    static member GetNodeOutput(n: FlowGraphNode) =
        List.ofSeq(snd(n.Output))
                                      
    static member SetNodeInput(node: FlowGraphNode, 
                               par: string,
                               input: FlowGraphNodeInputInfo) =        
        FlowGraphManager.RemoveNodeInput(node, par)  
        // Set dest
        if node.Input.ContainsKey(par) then
            node.Input.[par] <- input
        else
            node.Input.Add(par, input)
        // Set source
        match input.Input with
        | KernelOutput(n, returnIndex) ->
            if fst(n.Output) = null || (fst(n.Output) <> node) then
                n.Output <- (node, new Dictionary<int, string>())
                snd(n.Output).Add(returnIndex, par)
            else
                if snd(n.Output).ContainsKey(returnIndex) then
                    snd(n.Output).[returnIndex] <- par
                else
                    snd(n.Output).Add(returnIndex, par)
        | _ ->
            ()                                      
        
    static member RemoveNodeInput(node: FlowGraphNode, 
                                  par: string) =
        if node.Input.ContainsKey(par) then
            match node.Input.[par].Input with
            | KernelOutput(n, returnIndex) ->
                match n.Output with
                | _, mapping ->
                    mapping.Remove(returnIndex) |> ignore
            | _ ->
                ()                         
            node.Input.Remove(par) |> ignore      
            
    static member GetEntryPoints(root: FlowGraphNode) =
        let entries = new List<FlowGraphNode>([ root ])
        let mutable currentIndex = 0
        while(currentIndex < entries.Count) do
            let current = entries.[currentIndex]
            if current.IsEntryPoint then
                currentIndex <- currentIndex + 1
            else
                for item in FlowGraphManager.GetNodeInput(current) do
                    match item.Value.Input with
                    | KernelOutput(k, i) ->
                        entries.Add(k)
                    | _ ->
                        ()
                entries.RemoveAt(currentIndex)
        List.ofSeq(entries)    
        
    static member Flatten(root: FlowGraphNode) =
        let partialOrder = new List<FlowGraphNode>([ root ])
        let mutable currentIndex = 0
        while(currentIndex < partialOrder.Count) do
            let current = partialOrder.[currentIndex]
            for item in FlowGraphManager.GetNodeInput(current) do
                match item.Value.Input with
                | KernelOutput(k, i) ->
                    partialOrder.Add(k)
                | _ ->
                    ()
            currentIndex <- currentIndex + 1
        List.ofSeq(partialOrder)
        (*
    static member GetKernelNodes(id: FunctionInfoID,
                                 root: FlowGraphNode) =
        if root <> null then
            let nodes = new List<FlowGraphNode>()
            if root.KernelID = id then
                nodes.Add(root)
            for item in root.Input do
                match item.Value.Input with
                | KernelOutput(n, i) ->
                    nodes.AddRange(FlowGraphManager.GetKernelNodes(id, n))
                | _ ->
                    ()
            List.ofSeq(nodes)
        else
            []
            *)
         
            
        
                

