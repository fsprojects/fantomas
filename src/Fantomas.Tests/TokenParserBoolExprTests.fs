module Fantomas.Tests.TokenParserBoolExprTests

open Fantomas
open NUnit.Framework
open FsUnit
open Fantomas.TokenParser
open Fantomas.TokenParserBoolExpr
open Fantomas.Tests.TestHelper
open FsCheck

let getDefineExprs source =
    String.normalizeNewLine source
    |> TokenParser.getDefines
    |> snd
    |> getDefineExprs

[<Test>]
let ``Get define exprs from complex statements`` () =
    let source = """
#if !(INTERACTIVE || !(FOO && BAR) || BUZZ)
let x = 1
#endif
"""

    getDefineExprs source
    |> List.head
    |> should
        equal
        (BoolExpr.Not(
            BoolExpr.Or(
                BoolExpr.Ident "INTERACTIVE",
                BoolExpr.Or(
                    BoolExpr.Not
                    <| BoolExpr.And(BoolExpr.Ident "FOO", BoolExpr.Ident "BAR"),
                    BoolExpr.Ident "BUZZ"
                )
            )
        ))

[<Test>]
let ``Simple compiler directive - else expr`` () =
    let source = """
#if A
setupServer false
#if B
#else
setupServer true
#endif
#else
#endif
"""

    getDefineExprs source
    |> List.toArray
    |> should
        equal
        [| BoolExpr.Ident "A"
           BoolExpr.And(BoolExpr.Ident "B", BoolExpr.Ident "A")
           BoolExpr.Not
           <| BoolExpr.And(BoolExpr.Ident "B", BoolExpr.Ident "A")
           BoolExpr.Not <| BoolExpr.Ident "A" |]


[<Test>]
let ``CNF form of exprs from complex statements`` () =
    let source = """
#if !(INTERACTIVE || !(FOO || BAR) || BUZZ)
let x = 1
#endif
"""

    getDefineExprs source
    |> List.head
    |> BoolExpr.normalizeCNF
    |> should
        equal
        (BoolExpr.And(
            BoolExpr.Not(BoolExpr.Ident "INTERACTIVE"),
            BoolExpr.And(BoolExpr.Or(BoolExpr.Ident "FOO", BoolExpr.Ident "BAR"), BoolExpr.Not(BoolExpr.Ident "BUZZ"))
        ))

[<Test>]
let ``CNF flatten form of exprs from complex statements`` () =
    let source = """
#if !(INTERACTIVE || !(FOO || BAR) || BUZZ)
let x = 1
#endif
"""

    getDefineExprs source
    |> List.head
    |> BoolExpr.toFlatCNF
    |> List.toArray
    |> should
        equal
        [| set [ BoolExpr.Negative "INTERACTIVE" ]
           set [ BoolExpr.Positive "FOO"
                 BoolExpr.Positive "BAR" ]
           set [ BoolExpr.Negative "BUZZ" ] |]


[<Test>]
let ``BoolExpr SAT solve`` () =
    let getSource e =
        (sprintf "#if %s" e)
        + System.Environment.NewLine
        + "#endif"

    let test e x =
        getDefineExprs (getSource e)
        |> List.head
        |> BoolExpr.toFlatCNF
        |> BoolExpr.trySolveSAT FormatConfig.satSolveMaxStepsMaxSteps
        |> should equal x

    test "!(INTERACTIVE || !(FOO || BAR) || BUZZ)" (BoolExpr.Satisfiable [ "BAR"; "FOO" ])
    test "A && !A" BoolExpr.Unsatisfiable
    test "A && (!A || B) && (!B || !A)" BoolExpr.Unsatisfiable
    test "A && (!A || B) && (!B || !A || !C)" (BoolExpr.Satisfiable [ "A"; "B" ])

[<Test>]
let ``BoolExpr merge`` () =
    let getSource es =
        es
        |> Seq.map
            (fun e ->
                (sprintf "#if %s" e)
                + System.Environment.NewLine
                + "#endif")
        |> String.concat System.Environment.NewLine

    let test e x =
        getDefineExprs (getSource e)
        |> BoolExpr.mergeBoolExprs FormatConfig.satSolveMaxStepsMaxSteps
        |> List.map (snd >> List.toArray)
        |> List.toArray
        |> should equal (x |> List.map List.toArray |> List.toArray)

    test [ "A && B"; "A" ] [ [ "A"; "B" ] ]

    test [ "A && B"; "!A" ] [
        [ "A"; "B" ]
        []
    ]

    test [ "A || B"; "!A" ] [ [ "B" ] ]

    test [ "A || B"; "!A"; "!(A || B)" ] [
        []
        [ "B" ]
    ]

    test [ "A || B || C"; "!A"; "!C" ] [
        [ "B" ]
    ]

    test [ "A || A"; "!(A || A)" ] [
        [ "A" ]
        []
    ]

//--------------
// Property tests

let private verboseConf =
    { Config.Verbose with
          MaxTest = 500
          EndSize = 20
          Runner = NUnitRunner() }

let rec internal boolExprToString =
    function
    | BoolExpr.Ident x -> x
    | BoolExpr.Not (BoolExpr.Ident x) -> "!" + x
    | BoolExpr.Not e -> "!(" + boolExprToString e + ")"
    | BoolExpr.And (e1, e2) ->
        "("
        + boolExprToString e1
        + " && "
        + boolExprToString e2
        + ")"
    | BoolExpr.Or (e1, e2) ->
        "("
        + boolExprToString e1
        + " || "
        + boolExprToString e2
        + ")"

let internal boolExprsToSource es =
    es
    |> Seq.mapi
        (fun i e ->
            sprintf
                """#if %s
let x%i = %i
#else
let x%i_else = %i
#endif"""
                (boolExprToString e)
                i
                i
                i
                i)
    |> String.concat System.Environment.NewLine
    |> fun x -> x + System.Environment.NewLine

type BoolExprGenerator =
    static member SimpleIdent() =
        { new Arbitrary<string>() with
            member x.Generator =
                Gen.choose (int 'A', int 'Z')
                |> Gen.map (char >> string)

            member x.Shrinker t = Seq.empty }

[<Test>]
let ``Hash if expression parsing property`` () =
    Check.One(
        { verboseConf with
              Arbitrary = [ typeof<BoolExprGenerator> ] },
        (fun e ->
            let source = boolExprsToSource [ e ]

            getDefineExprs source
            |> List.head
            |> should equal e)
    )

[<Test>]
let ``Hash if expression normalize property`` () =
    Check.One(
        { verboseConf with
              Arbitrary = [ typeof<BoolExprGenerator> ] },
        (fun e ->
            let checkNormalize =
                function
                | BoolExpr.Not (BoolExpr.Not _)
                | BoolExpr.Not (BoolExpr.And _)
                | BoolExpr.Not (BoolExpr.Or _)
                | BoolExpr.Or (_, BoolExpr.And _)
                | BoolExpr.Or (BoolExpr.And _, _) -> false
                | _ -> true

            let source = boolExprsToSource [ e ]

            getDefineExprs source
            |> List.head
            |> BoolExpr.normalizeCNF
            |> BoolExpr.forall checkNormalize)
    )

let isSatisfiable e =
    match BoolExpr.trySolveSAT FormatConfig.satSolveMaxStepsMaxSteps (BoolExpr.toFlatCNF e) with
    | BoolExpr.Satisfiable _ -> true
    | _ -> false

[<Test>]
let ``Hash ifs optimize defines property`` () =
    Check.One(
        { verboseConf with
              Arbitrary = [ typeof<BoolExprGenerator> ] },
        (fun es ->
            let source = boolExprsToSource es
            let _, hashTokens = getDefines source
            let allDefines = getDefinesWords hashTokens
            let defines = getOptimizedDefinesSets hashTokens

            let definesToLiterals ds =
                let s = set ds

                allDefines
                |> List.map
                    (fun x ->
                        if Set.contains x s then
                            BoolExpr.Positive x
                        else
                            BoolExpr.Negative x)

            es
            |> List.collect (fun e -> [ e; BoolExpr.Not e ])
            |> List.filter isSatisfiable
            |> List.forall
                (fun e ->
                    defines
                    |> List.exists
                        (fun ds ->
                            BoolExpr.toFlatCNF e
                            |> fun cnf -> BoolExpr.eval cnf (definesToLiterals ds))))
    )

[<Test>]
let ``Hash ifs source format property`` () =
    Check.One(
        { verboseConf with
              MaxTest = 100
              Arbitrary = [ typeof<BoolExprGenerator> ] },
        (fun (es: list<_>) ->
            (es
             |> List.forall (fun e -> isSatisfiable e && isSatisfiable (BoolExpr.Not e)))
            ==> lazy
                (let source = boolExprsToSource es
                 let result = formatSourceString false source config

                 if String.isNotNullOrEmpty result then
                     result |> should equal source))
    )

[<Test>]
let ``get define exprs from unit test with defines in triple quote string`` () =
    let source = "
\"\"\"
#if FOO
#if BAR
#endif
#endif
\"\"\"
"
    getDefineExprs source == List<BoolExpr>.Empty

[<Test>]
let ``nested quote in triple quote string should not yield defines`` () =
    let source = "
\"\"\"
\"
#if FOO
#if BAR
#endif
#endif
\"
\"\"\"
"
    getDefineExprs source == List<BoolExpr>.Empty
