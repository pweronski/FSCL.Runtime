namespace FSCL.Runtime.KernelDiscovery

open System
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open FSCL.Compiler.Core.Util

[<assembly:DefaultComponentAssembly>]
do()

[<KernelDiscoveryProcessor("FSCL_REFERENCE_DISCOVERY_PROCESSOR", Dependencies = [| "FSCL_CALL_EXPRESSION_DISCOVERY_PROCESSOR" |])>]
type KernelReferenceDiscovery() =      
    interface IKernelDiscoveryProcessor with
    
        member this.Run(obj, step) =
            if (obj :? Expr) then
                match QuotationAnalysis.GetKernelFromName(obj :?> Expr) with
                | Some(mi, b) -> 
                    // Create singleton kernel call graph
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
            