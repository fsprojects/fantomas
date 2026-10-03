namespace PmaBolero.Client

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Microsoft.Extensions.DependencyInjection.Extensions
open Bolero.Remoting.Client

module Program =

    [<EntryPoint>]
    let Main args =
        let builder = WebAssemblyHostBuilder.CreateDefault(args)
        builder.RootComponents.Add<Pages.Main.MyApp>("#main")
        builder.Services
            .AddRemoting(builder.HostEnvironment)
            .Services
#if (!DEBUG)
            .RemoveAll<Microsoft.Extensions.Http.IHttpMessageHandlerBuilderFilter>()
#endif
            |> ignore
        builder.Build().RunAsync() |> ignore
        0
