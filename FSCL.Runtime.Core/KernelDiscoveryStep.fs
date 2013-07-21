namespace FSCL.Runtime

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

///
///<summary>
///The base type of every compiler step processor
///</summary>
///<remarks>
///Developers of step processors should not inherit from this class but from the generic CompilerStepProcessor and provide an implementation to the method "Run"
///</remarks>
///
type IKernelDiscoveryProcessor =   
    ///
    ///<summary>
    ///The method to be called to execute the processor
    ///</summary>
    ///<remarks>
    ///This method looks for a method called "Run" in the runtime time definition of this instance and invokes it using the provided parameter
    ///</remarks>
    ///<param name="obj">The input of the processor</param>
    ///<param name="owner">The owner step</param>
    ///<returns>The output produced by this processor</returns>
    /// 
    abstract member Run: obj * KernelDiscoveryStep -> RuntimeCallGraph option

///
///<summary>
///The base type of every compiler step
///</summary>
///<remarks>
///Developers of steps should not inherit from this class but from the generic CompilerStep and provide an implementation to the method "Run"
///</remarks>
///
and KernelDiscoveryStep(processors:IKernelDiscoveryProcessor list) =
    ///
    ///<summary>
    ///The set of step processors
    ///</summary>
    /// 
    member val Processors = processors with get
    ///
    ///<summary>
    ///The method to be called to execute the step
    ///</summary>
    ///<remarks>
    ///This method looks for a method called "Run" in the runtime time definition of this instance and invokes it using the provided parameter
    ///</remarks>
    ///<param name="obj">The input of the step</param>
    ///<returns>The output produced by this step</returns>
    /// 
    member this.Run(obj) =        
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < processors.Length) do
            output <- processors.[index].Run(obj, this)
            index <- index + 1
        if output.IsNone then
            raise (KernelDiscoveryException("The runtime is not able to recognize the kernels inside [" + obj.ToString() + "]"))
        output.Value
        