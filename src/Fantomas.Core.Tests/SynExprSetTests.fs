module Fantomas.Core.Tests.SynExprSetTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

/// See https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-synexpr.html#Set

[<Test>]
let ``array indexer set`` () =
    formatSourceString
        """
let arr = [|0|]
(arr.[0]) <- 1
"""
        config
    |> should
        equal
        """let arr = [| 0 |]
(arr.[0]) <- 1
"""

[<Test>]
let ``setter of type set`` () =
    formatSourceString
        """
type T() =
    member val X = 0 with get, set
(T().X) <- 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T() =
    member val X = 0 with get, set

(T().X) <- 1
"""

[<Test>]
let ``mutable value set`` () =
    formatSourceString
        """
let mutable x = 0
(x) <- 1
"""
        config
    |> should
        equal
        """let mutable x = 0
(x) <- 1
"""

[<Test>]
let ``don't add additional new line after SynExpr.LongIndentSet, 1111`` () =
    formatSourceString
        """
        let options =
            jsOptions<Vis.Options> (fun o ->
                o.autoResize <- Some true
                o.edges <- Some(jsOptions<Vis.EdgeOptions> (fun e -> e.arrows <- Some <| U2.Case1 "to"))

                o.interaction <-
                    Some
                        (createObj [ "hover" ==> true
                                     "zoomView" ==> true
                                     "hoverConnectedEdges" ==> false ])

                o.layout <- Some(createObj [ "randomSeed" ==> 0 ])

                let hierOpts dir =
                    createObj [ "enabled" ==> true
                                "levelSeparation" ==> 170
                                "nodeSpacing" ==> 100
                                "treeSpacing" ==> 100
                                "direction" ==> dir ]

                let layout =
                    match opts.Layout with
                    | Graph.Free -> createObj []
                    | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
                    | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

                o.layout <- Some layout)
"""
        { config with
            MaxArrayOrListWidth = 40
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let options =
    jsOptions<Vis.Options> (fun o ->
        o.autoResize <- Some true
        o.edges <- Some(jsOptions<Vis.EdgeOptions> (fun e -> e.arrows <- Some <| U2.Case1 "to"))

        o.interaction <-
            Some(
                createObj
                    [ "hover" ==> true
                      "zoomView" ==> true
                      "hoverConnectedEdges" ==> false ]
            )

        o.layout <- Some(createObj [ "randomSeed" ==> 0 ])

        let hierOpts dir =
            createObj
                [ "enabled" ==> true
                  "levelSeparation" ==> 170
                  "nodeSpacing" ==> 100
                  "treeSpacing" ==> 100
                  "direction" ==> dir ]

        let layout =
            match opts.Layout with
            | Graph.Free -> createObj []
            | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
            | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

        o.layout <- Some layout)
"""

[<Test>]
let ``multi line assign mutable setter assignment, 659`` () =
    formatSourceString
        """
ctx.Response.Headers.[HeaderNames.ContentType] <- Constants.jsonApiMediaType
                                                  |> StringValues
ctx.Response.Headers.[HeaderNames.ContentLength] <- bytes.Length
                                                    |> string
                                                    |> StringValues
ctx.Response.SomeElseThatIsMutable <- [ "a"; "b"; "c" ]
                                      |> List.indexed
                                      |> List.map snd
"""
        { config with
            MaxLineLength = 80
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
ctx.Response.Headers.[HeaderNames.ContentType] <-
    Constants.jsonApiMediaType |> StringValues

ctx.Response.Headers.[HeaderNames.ContentLength] <-
    bytes.Length |> string |> StringValues

ctx.Response.SomeElseThatIsMutable <-
    [ "a"; "b"; "c" ] |> List.indexed |> List.map snd
"""

[<Test>]
let ``multi line NamedIndexedPropertySet`` () =
    formatSourceString
        """
 HttpContext.Response.Body(128) <- bytes.Length
                                   |> string
                                   |> StringValues
"""
        { config with
            MaxInfixOperatorExpression = 10 }
    |> prepend newline
    |> should
        equal
        """
HttpContext.Response.Body(128) <-
    bytes.Length
    |> string
    |> StringValues
"""

[<Test>]
let ``multi line DotNamedIndexedPropertySet`` () =
    formatSourceString
        """
 (HttpContextResponse).Body(128) <- bytes.Length
                                    |> string
                                    |> StringValues
"""
        { config with
            MaxInfixOperatorExpression = 10 }
    |> prepend newline
    |> should
        equal
        """
(HttpContextResponse).Body(128) <-
    bytes.Length
    |> string
    |> StringValues
"""

[<Test>]
let ``keep new line before SynExpr.DotIndexedSet, 1314`` () =
    formatSourceString
        """
          match x with
          | NotificationEvent.Lint (file, warnings) ->
              let uri = Path.FilePathToUri file

              diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], (fun _ _ -> [||]))
              |> ignore

              let fs =
                warnings
                |> List.choose (fun w ->
                   w.Warning.Details.SuggestedFix
                   |> Option.bind
                        (fun f ->
                          let f = f.Force()
                          let range = fcsRangeToLsp w.Warning.Details.Range

                          f
                          |> Option.map (fun f -> range, { Range = range; NewText = f.ToText })))

              lintFixes.[uri] <- fs
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
match x with
| NotificationEvent.Lint(file, warnings) ->
    let uri = Path.FilePathToUri file

    diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], (fun _ _ -> [||]))
    |> ignore

    let fs =
        warnings
        |> List.choose (fun w ->
            w.Warning.Details.SuggestedFix
            |> Option.bind (fun f ->
                let f = f.Force()
                let range = fcsRangeToLsp w.Warning.Details.Range

                f
                |> Option.map (fun f -> range, { Range = range; NewText = f.ToText })))

    lintFixes.[uri] <- fs
"""

[<Test>]
let ``space before uppercase invocation with TypeApp`` () =
    formatSourceString
        """
Log.Logger <-
    LoggerConfiguration<Foo>()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
Log.Logger <-
    LoggerConfiguration<Foo>()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger ()
"""

[<Test>]
let ``const in NamedIndexedPropertySet, 2498`` () =
    formatSourceString
        """
let xs = [| 42 |]

xs.Items.Item 0 <- 20
"""
        config
    |> prepend newline
    |> should
        equal
        """
let xs = [| 42 |]

xs.Items.Item 0 <- 20
"""
