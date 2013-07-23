namespace FSCL.Runtime

open System

///
///<summary>
///The attribute used to describe a compiler step
///</summary>
/// 
[<AllowNullLiteral>]
type KernelDiscoveryProcessorAttribute(i: string) =
    inherit Attribute()    
    ///
    ///<summary>
    ///The global ID of the step
    ///</summary>
    /// 
    member val ID = i with get
    ///
    ///<summary>
    ///The set of dependencies (i.e. the set of steps that must be executed before this one)
    ///</summary>
    /// 
    member val Dependencies: string array = [||] with get, set
    ///
    ///<summary>
    ///The set of steps that must be executed after this one
    ///</summary>
    /// 
    member val Before: string array = [||] with get, set
        
[<AllowNullLiteral>]          
[<AttributeUsage(AttributeTargets.Assembly)>]
type DefaultComponentAssembly() =
    inherit Attribute()
    
// Kernel run mode
type KernelRunningMode =
| OpenCL
| Multithread
| Sequential

[<AllowNullLiteral>]
type DeviceAttribute(platform: int, device: int) =
    inherit Attribute()

    member val Platform = platform with get
    member val Device = device with get

  
