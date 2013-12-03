open FSCL.Compiler.KernelLanguage
open FSCL.Runtime
open System.Reflection
open System.Reflection.Emit
open FSCL.Runtime.KernelRunner
open System
        
// Vector addition
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
    
[<EntryPoint>]
let main argv =
    let size = 1024

    // Create arrays
    let a = Array.create size 2.0f
    let b = Array.create size 3.0f
    let c = Array.zeroCreate<float32> (size)

    // Execute vector add in OpenCL mode
    <@ VectorAdd(a, b, c) @>.Run(a.Length, 64) |> ignore

    // Return
    0