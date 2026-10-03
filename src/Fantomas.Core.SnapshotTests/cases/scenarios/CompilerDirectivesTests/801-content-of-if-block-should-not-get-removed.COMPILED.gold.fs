#if !COMPILED



#endif

#load "../shared/utilities.fsx"

open Vspan.Common.Utilities

#load "DialoutFunction.fsx"

open Vspan.Domain.Functions

let Run (message: string, executionContext: ExecutionContext, log: TraceWriter) =
    let logInfo = log.Info
    let getSetting = System.Environment.GetEnvironmentVariable

    executionContext |> FunctionGuid logInfo |> ignore

    message
    |> Out.Dialout.DialoutFunction.Accept logInfo getSetting
