module Program

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Serilog
open Startup

[<EntryPoint>]
let main args =
    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun builder ->
            builder.CaptureStartupErrors(true).UseSerilog(dispose = true).UseStartup<Startup>()
            |> ignore)
        .Build()
        .Run()

    0
