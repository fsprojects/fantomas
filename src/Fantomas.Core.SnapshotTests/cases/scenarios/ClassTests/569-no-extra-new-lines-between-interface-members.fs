(*---
fsharp_max_function_binding_width = 80
fsharp_multiline_bracket_style = cramped
---*)
namespace Quartz.Fsharp

module Logging =
    open Quartz.Logging
    open System

    //todo: it seems that quartz doesn't use mapped and nested context,
    //however, check if this is the best implementation for this interface
    type private QuartzLoggerWrapper(f) =
        interface ILogProvider with

            member this.OpenMappedContext(_, _) =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.OpenNestedContext _ =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.GetLogger _name = new Logger(f)

    let SetQuartzLoggingFunction f =
        let loggerFunction level (func: Func<string>) exc parameters =
            let wrappedFunction =
                Helpers.nullValuesToOptions (fun (x: Func<string>) -> (fun () -> x.Invoke())) func
            let wrappedException = Helpers.nullValuesToOptions id exc
            f level wrappedFunction wrappedException (parameters |> List.ofArray)

        LogProvider.SetCurrentLogProvider(QuartzLoggerWrapper(loggerFunction))

    let SetQuartzLogger l = LogProvider.SetCurrentLogProvider(l)
