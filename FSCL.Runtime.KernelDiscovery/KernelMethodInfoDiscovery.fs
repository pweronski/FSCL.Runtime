namespace FSCL.Runtime.KernelDiscovery

open System
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open FSCL.Compiler.Core.Util

[<assembly:DefaultComponentAssembly>]
do()

[<KernelDiscoveryProcessor("FSCL_METHOD_INFO_DISCOVERY_PROCESSOR")>]
type KernelMethodInfoDiscovery() =      
    interface IKernelDiscoveryProcessor with
    
        member this.Run(obj, step) =
            if (obj :? MethodInfo) then
                let mi = obj :?> MethodInfo
                match mi with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->               
                    let cg = new RuntimeCallGraph()
                    cg.Add(mi)
                    // Device attribute
                    let device = mi.GetCustomAttribute(typeof<DeviceAttribute>) 
                    if device <> null then
                        cg.Get(mi).Device <- device :?> DeviceAttribute
                    Some(cg)
                | _ ->
                    None
            else
                None
            