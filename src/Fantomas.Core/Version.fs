module internal Fantomas.Core.Version

open System.Reflection

let fantomasVersion =
    lazy
        (let assembly = typeof<SyntaxOak.Node>.Assembly

         assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
         |> Option.ofObj
         |> Option.map (fun a -> a.InformationalVersion)
         |> Option.defaultValue (Assembly.GetExecutingAssembly().GetName().Version.ToString()))
