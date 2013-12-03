open FSCL.Runtime
open FSCL.Runtime.Metric
open FSCL.Runtime.Metric.DataTransferMetric
open System.Reflection
open System.Reflection.Emit
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
        
[<EntryPoint>]
let main argv =
    let metric = new DataTransferMetric()
    metric.MinSize <- 16 <<< 20
    metric.MaxSize <- (16 <<< 20) + 1
    metric.PerStepDuration <- 3000
    metric.Step <- 2
    metric.IsStepMultiplier <- true
    metric.DumpFolder <- Some("MetricResult")
    metric.Validate <- false
    metric.Debug <- true
    metric.Profile(0, 0) |> ignore
    metric.Profile(0, 1) |> ignore
    0


    
