module Fantomas.Tests.LambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``keep comment after arrow`` () =
    formatSourceString false """_Target "FSharpTypesDotNet" (fun _ -> // obsolete
 ())
"""  ({ config with IndentSize = 2; MaxLineLength = 90 })
    |> prepend newline
    |> should equal """
_Target "FSharpTypesDotNet" (fun _ -> // obsolete
  ())
"""

let ``indent multiline lambda in parenthesis, 523`` () =
    formatSourceString false """let square = (fun b ->
    b*b
    prinftn "%i" b*b
)
"""  config
    |> prepend newline
    |> should equal """
let square =
    (fun b ->
        b * b
        prinftn "%i" b * b)
"""

[<Test>]
let ``lambda inside tupled argument`` () =
    formatSourceString false """#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>
        ((fun _ ->
            fragment []
                [ h3 []
                      [ str "Heading "
                        Badge.badge [ Badge.Color Secondary ] [ str "New" ] ]
                  Badge.badge [ Badge.Color Warning ] [ str "oh my" ]]), "BadgeSample")

exportDefault badgeSample
"""  config
    |> prepend newline
    |> should equal """
#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>
        ((fun _ ->
            fragment [] [
                h3 [] [
                    str "Heading "
                    Badge.badge [ Badge.Color Secondary ] [
                        str "New"
                    ]
                ]
                Badge.badge [ Badge.Color Warning ] [
                    str "oh my"
                ]
            ]),
         "BadgeSample")

exportDefault badgeSample
"""

[<Test>]
let ``long identifier inside lambda`` () =
    formatSourceString false """
let a =
    b
    |> List.exists (fun p ->
        x && someVeryLongIdentifierrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrzzzz___________)
"""  ({ config with MaxLineLength = 80})
    |> prepend newline
    |> should equal """
let a =
    b
    |> List.exists (fun p ->
        x
        && someVeryLongIdentifierrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrzzzz___________)
"""

[<Test>]
let ``FAKE target`` () =
    formatSourceString false """
Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj" ]
    |> List.iter Shell.cleanDir
)
"""  config
    |> prepend newline
    |> should equal """
Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj" ]
    |> List.iter Shell.cleanDir)
"""

[<Test>]
let ``destructed argument lamba`` () =
    formatSourceString false """
List.filter (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
"""  config
    |> prepend newline
    |> should equal """
List.filter (fun { ContentBefore = contentBefore } ->
    // some comment
    let a = 8
    let b = List.length contentBefore
    a + b)
"""

[<Test>]
let ``destructed argument lamba in let binding`` () =
    formatSourceString false """
let a =
    (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
"""  config
    |> prepend newline
    |> should equal """
let a =
    (fun { ContentBefore = contentBefore } ->
        // some comment
        let a = 8
        let b = List.length contentBefore
        a + b)
"""


[<Test>]
let ``indent when identifier is smaller than ident size`` () =
    formatSourceString false """
foo (fun a ->
                let b = 8
                b)
"""  config
    |> prepend newline
    |> should equal """
foo (fun a ->
        let b = 8
        b)
"""

[<Test>]
let ``short ident in nested let binding`` () =
    formatSourceString false """let a =
    foo (fun a ->
                let b = 8
                b)
"""  ({ config with IndentSize = 2})
    |> prepend newline
    |> should equal """
let a =
  foo (fun a ->
    let b = 8
    b)
"""

[<Test>]
let ``longer ident in nested let binding`` () =
    formatSourceString false """let a =
    foobar (fun a ->
                let b = 8
                b)
"""  config
    |> prepend newline
    |> should equal """
let a =
    foobar (fun a ->
        let b = 8
        b)
"""

[<Test>]
let ``multiple braces should add indent`` () =
    formatSourceString false """((((fun () ->
    printfn "meh"
    ()))))
"""  config
    |> prepend newline
    |> should equal """
((((fun () ->
    printfn "meh"
    ()))))
"""

[<Test>]
let ``add space after chained ident, 676`` () =
    formatSourceString false """let foo = Foo(fun () -> Foo.Create x).Value"""  config
    |> prepend newline
    |> should equal """
let foo = Foo(fun () -> Foo.Create x).Value
"""

[<Test>]
let ``line comment after lambda should not necessary make it multiline`` () =
    formatSourceString false """let a = fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
"""  ({ config with MaxFunctionBindingWidth = 150 })
    |> prepend newline
    |> should equal """
let a = fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
"""


[<Test>]
let ``multiline let binding in lambda`` () =
    formatSourceString false """
CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
            let connectionString =
                if hostedService then RoleEnvironment.GetConfigurationSettingValue(configName)
                else ConfigurationManager.ConnectionStrings.[configName].ConnectionString
            configSettingPublisher.Invoke(connectionString) |> ignore)
"""  config
    |> prepend newline
    |> should equal """
CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
    let connectionString =
        if hostedService
        then RoleEnvironment.GetConfigurationSettingValue(configName)
        else ConfigurationManager.ConnectionStrings.[configName].ConnectionString

    configSettingPublisher.Invoke(connectionString)
    |> ignore)
"""

[<Test>]
let ``line comment after arrow should not introduce additional newline, 772`` () =
    formatSourceString false """let genMemberFlagsForMemberBinding astContext (mf: MemberFlags) (rangeOfBindingAndRhs: range) =
    fun ctx ->
        match mf with
        | MFOverride _ ->
            (fun (ctx: Context) -> // trying to get AST trivia

                ctx.Trivia
                |> List.tryFind (fun { Type = t; Range = r } -> // trying to get token trivia

                    match t with
                    | MainNode "SynMemberDefn.Member" -> RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                    | Token { TokenInfo = { TokenName = "MEMBER" } } -> r.StartLine = rangeOfBindingAndRhs.StartLine

                    | _ -> false)
                |> Option.defaultValue (!- "override ")
                <| ctx)
        <| ctx
"""  config
    |> prepend newline
    |> should equal """
let genMemberFlagsForMemberBinding astContext (mf: MemberFlags) (rangeOfBindingAndRhs: range) =
    fun ctx ->
        match mf with
        | MFOverride _ ->
            (fun (ctx: Context) -> // trying to get AST trivia

            ctx.Trivia
            |> List.tryFind (fun { Type = t; Range = r } -> // trying to get token trivia

                match t with
                | MainNode "SynMemberDefn.Member" -> RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                | Token { TokenInfo = { TokenName = "MEMBER" } } -> r.StartLine = rangeOfBindingAndRhs.StartLine

                | _ -> false)
            |> Option.defaultValue (!- "override ")
            <| ctx)
        <| ctx
"""

[<Test>]
let ``line comment after arrow should not introduce extra newline`` () =
    formatSourceString false """List.tryFind (fun { Type = t; Range = r } -> // foo
                    let a = 8
                    a + 9)
"""  config
    |> prepend newline
    |> should equal """
List.tryFind (fun { Type = t; Range = r } -> // foo
    let a = 8
    a + 9)
"""

[<Test>]
let ``lambda body should be indented far enough, 870`` () =
    formatSourceString false """  let projectIntoMap projection =
    fun state eventEnvelope ->
      state
      |> Map.tryFind eventEnvelope.Metadata.Source
      |> Option.defaultValue projection.Init
      |> fun projectionState -> eventEnvelope.Event |> projection.Update projectionState
      |> fun newState -> state |> Map.add eventEnvelope.Metadata.Source newState

let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
         eventEnvelope.Event
         |> projection.Update projectionState
    |> fun newState ->
         state
         |> Map.add eventEnvelope.Metadata.Source newState
"""
        ({ config with
               IndentSize = 2
               SpaceBeforeUppercaseInvocation = true
               SpaceBeforeColon = true
               SpaceAfterComma = false
               SpaceAroundDelimiter = false
               MaxInfixOperatorExpression = 40
               MaxFunctionBindingWidth = 60
               MultilineBlockBracketsOnSameColumn = true })
    |> prepend newline
    |> should equal """
let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
         eventEnvelope.Event
         |> projection.Update projectionState
    |> fun newState ->
         state
         |> Map.add eventEnvelope.Metadata.Source newState

let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
         eventEnvelope.Event
         |> projection.Update projectionState
    |> fun newState ->
         state
         |> Map.add eventEnvelope.Metadata.Source newState
"""
