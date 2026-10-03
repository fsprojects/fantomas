#if !COMPILED

#I "C:/devuser/AppData/Roaming/npm/node_modules/azure-functions-core-tools/bin"
#r "Microsoft.Azure.WebJobs.Host.dll"
#r "System.Net.Http.Formatting.dll"

open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Host

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
