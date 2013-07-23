namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open FSCL.Compiler
open FSCL.Runtime.Metric
open System

type internal KernelParameterTable = Dictionary<String, KernelParameterInfo>

type internal KernelManager(compiler: FSCL.Compiler.Compiler, metric: SchedulingMetric option) =       
    // The data structure caching devices (queues, contexts) and compiled kernels 
    member val GlobalCache = new RuntimeCache() with get
    // The metric used to select the best devices for a kernel
    member val SchedulingMetric = metric with get             
    // The FSCL compiler   
    member val Compiler = compiler with get
    // The multithread adaptor (compiler) to run kernels using multithreading
    member val KernelAdaptor = new MultithreadKernelAdaptor() with get
         
    // Utility function to store kernels found all around the assembly. Called by the constructor
    member private this.StoreKernel(node:CallGraphNode, multithread: bool, platformIndex, deviceIndex) =
        let mutable device = null
        let mutable kernel = null
        let mutable compiledKernel = null

        //#1: Check if there is the corresponding device info have been set
        if not (this.GlobalCache.Devices.ContainsKey(platformIndex, deviceIndex)) then        
            // Get OpenCL info
            let platform = ComputePlatform.Platforms.[platformIndex]
            let dev = platform.Devices.[deviceIndex]   
            let devs = new System.Collections.Generic.List<ComputeDevice>();
            devs.Add(dev)
            // Create context and queue
            let contextProperties = new ComputeContextPropertyList(platform)
            let computeContext = new ComputeContext(devs, contextProperties, null, System.IntPtr.Zero) 
            let computeQueue = new ComputeCommandQueue(computeContext, dev, ComputeCommandQueueFlags.None) 
            // Add device to the global devices cache
            this.GlobalCache.Devices.Add((platformIndex, deviceIndex), new RuntimeDeviceData(dev, computeContext, computeQueue))
            device <- this.GlobalCache.Devices.[platformIndex, deviceIndex]

        //#2: Check if the kernel has been already FSCL-compiled
        if not multithread then
            if not (this.GlobalCache.Kernels.ContainsKey(node.ID)) then
                // FSCL-compile
                let (kernelModule, conversionData) = this.Compiler.Compile(node.ParsingExpression) :?> (KernelModule * string)
                this.GlobalCache.Kernels.Add(node.ID, new RuntimeKernelData(kernelModule.Kernel, None, Some(conversionData)))
            else if this.GlobalCache.Kernels.[node.ID].OpenCLCode.IsNone then
                // FSCL-compile
                let (kernelModule, conversionData) = this.Compiler.Compile(node.ParsingExpression) :?> (KernelModule * string)
                this.GlobalCache.Kernels.[node.ID].OpenCLCode <- Some(conversionData)
        else
            if not (this.GlobalCache.Kernels.ContainsKey(node.ID)) then
                // Create multithread version
                let mtKernel = this.KernelAdaptor.CreateMultithreadKernel(node.ID)
                this.GlobalCache.Kernels.Add(node.ID, new RuntimeKernelData(null, Some(mtKernel), None))
            else if this.GlobalCache.Kernels.[node.ID].MultithreadVersion.IsNone then
                // Create multithread version
                let mtKernel = this.KernelAdaptor.CreateMultithreadKernel(node.ID)
                this.GlobalCache.Kernels.[node.ID].MultithreadVersion <- Some(mtKernel)
        kernel <- this.GlobalCache.Kernels.[node.ID]
                
        //#3: If OpenCL mode, check if the device target code has been already generated    
        if not multithread then        
            if not (this.GlobalCache.Kernels.[node.ID].Instances.ContainsKey(platformIndex, deviceIndex)) then
                let computeProgram = new ComputeProgram(device.Context, kernel.OpenCLCode.Value)
                try
                    computeProgram.Build([| device.Device |], "", null, System.IntPtr.Zero)
                with
                | ex -> 
                    let log = computeProgram.GetBuildLog(device.Device)
                    raise (new KernelCompilationException("Device code generation failed: " + log))
                // Create kernel
                let computeKernel = computeProgram.CreateKernel(kernel.Info.ID.Name)
                // Add kernel implementation to the list of implementations for the given kernel
                let compiledKernel = new RuntimeCompiledKernelData(computeProgram, computeKernel)
                kernel.Instances.Add((platformIndex, deviceIndex), compiledKernel)
            compiledKernel <- kernel.Instances.[platformIndex, deviceIndex]
          
        //#4: Return device, kernel and compiled kernel
        (device, kernel, compiledKernel)
       
    member private this.AnalyzeAndStoreKernel(kernel:CallGraphNode, mode: KernelRunningMode, fallback: bool) =
        // Check if OpenCL enabled platform (at least one opencl platform with one device)
        let kernelModule = ref None
        if KernelManagerTools.IsOpenCLAvailable() && mode = KernelRunningMode.OpenCL then
            // Check if a particular device is specified by the user via KernelAttribute
            if kernel.Device <> null then
                // Check if platform and device indexes are valid  
                if ComputePlatform.Platforms.Count <= kernel.Device.Platform || (ComputePlatform.Platforms.[kernel.Device.Platform]).Devices.Count <= kernel.Device.Device then
                    raise (new KernelCompilationException("The platform and device indexes specified for the kernel " + kernel.ID.Name + " are invalid"))
                this.StoreKernel(kernel, false, kernel.Device.Platform, kernel.Device.Device)
            // No statically determined device: build kernel for all the possible devices
            else
                // The heart: find best device using a metric (by now fixed assignment)
                this.StoreKernel(kernel, false, 0, 0)
        // If opencl not available or multithread/sequential execution required
        else 
            if mode = KernelRunningMode.OpenCL && (not fallback) then
                raise (KernelSchedulingException("No OpenCL device is available in the system. Please check the functionality of your devices and that OpenCL is properly installed in the system"))
            if mode = KernelRunningMode.OpenCL && fallback then
                Console.WriteLine("Warning: kernel " + kernel.ID.Name + " is fallbacking to multithread execution")
            this.StoreKernel(kernel, true, 0, 0)
        
    member this.CompileAndCache(graph: RuntimeCallGraph, 
                                mode: KernelRunningMode, 
                                fallback: bool) =  
        let multithread = (mode <> KernelRunningMode.OpenCL)    
        for k in graph.Kernels do
            // Check if already compiled
            k.RuntimeCacheData <- Some(this.AnalyzeAndStoreKernel(k, mode, fallback))
