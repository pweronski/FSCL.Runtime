namespace FSCL.Runtime

open System
open System.Collections.Generic
open FSCL.Runtime
open FSCL.Compiler.Tools.GraphUtil
open FSCL.Compiler
open System.Reflection

exception RuntimerBuildException of string
    
type internal RuntimeBuilder() =
    static member Build(conf: RuntimeConfiguration) =
        let processors = new Dictionary<string, KernelDiscoveryProcessorConfiguration>()
        
        // Explode sources and group by component type (conf must be explicit)
        for s in conf.Sources do
            for sp in s.KernelDiscoveryProcessors do
                processors.Add(sp.ID, sp)
        
        // Check that each processors has and owner step and a before/after processor
        for p in processors do
            for dep in p.Value.Dependencies do
                if not (processors.ContainsKey(dep)) then
                    raise (Exception("The runtime processor " + p.Key + " requires processor " + dep + " but this processor has not been found"))
                               
        // Foreach step, create graph of processors
        let procGraphs = new Graph<KernelDiscoveryProcessorConfiguration, string>()
        for p in processors do
            procGraphs.Add(p.Key, p.Value) |> ignore
        for p in processors do
            let step = procGraphs.Get(p.Value.ID).Value
            for d in p.Value.Dependencies do
                procGraphs.Connect(d, p.Value.ID)
            for d in p.Value.Before do
                if processors.ContainsKey(d) then
                    procGraphs.Connect(p.Value.ID, d)

        // Topological sort of processors
        let sorted = procGraphs.Sorted
        let processors = seq {
            if sorted.IsNone then
                raise (Exception("Cannot build an ordered list using the specified processors since there is a cycle in steps dependencies"))
            for (id, data) in sorted.Value do
                yield data.Type.GetConstructor([||]).Invoke([||]) :?> IKernelDiscoveryProcessor
        }
        let flatSteps = List.ofSeq(processors)
        flatSteps
      
            
        
        




