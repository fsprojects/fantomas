module Fantomas.Tests.CodeFormatterExtTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``line, file and path identifiers``() =
    formatSourceString """
    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()
    """ config
    |> prepend newline
    |> should equal """
let printSourceLocation() = 
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__
printSourceLocation()"""

[<Test>]
let ``async workflows``() =
    formatSourceString """
let fetchAsync(name, url:string) =
    async { 
        try 
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
            | ex -> printfn "%s" (ex.Message);
    }
    """ config
    |> prepend newline
    |> should equal """
let fetchAsync(name, url : string) = 
    async { 
        try 
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
        | ex -> printfn "%s" (ex.Message) }"""

[<Test>]
let ``computation expressions``() =
    formatSourceString """
let comp =
    eventually { for x in 1 .. 2 do
                    printfn " x = %d" x
                 return 3 + 4 }""" config
    |> prepend newline
    |> should equal """
let comp = 
    eventually { 
        for x in 1..2 do
            printfn " x = %d" x
        return 3 + 4 }"""

