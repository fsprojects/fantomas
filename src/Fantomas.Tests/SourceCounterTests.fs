module Fantomas.Tests.SourceCounterTests

open Fantomas
open Fantomas.SourceParser
open Fantomas.SourceCounter
open NUnit.Framework
open Fantomas.Tests.TestHelper

let private getAst input = parse false input |> Array.head |> fst

let private (|SingleModuleDeclarationInFile|_|) ast =
    match ast with
    | ImplFile (ParsedImplFileInput (_, [ ModuleOrNamespace (_, _, _, _, [ mdl ], _, _) ])) -> Some mdl
    | _ -> None

let private (|SingleLetBindingInFile|_|) ast =
    match ast with
    | SingleModuleDeclarationInFile (Let (LetBinding (_, _, _, _, _, pat, expression, _))) -> Some(pat, expression)
    | _ -> None

[<Test>]
let ``count parameters of lambda`` () =
    let parenExpr =
        """
(fun aaa bbbbbb (CCC ccc) ddddddddddd (EEEE eee) (FFF fff) ggggg ->
                failwith "Not important")
"""
        |> Input
        |> toSynExprs
        |> List.head

    let result =
        match parenExpr with
        | Paren (DesugaredLambda (cps, _)) ->
            CountAstNode.ComplexPatsList cps
            |> isASTLongerThan 40
        | _ -> failwithf "expected different ast"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result

[<Test>]
let ``count function signature`` () =
    let ast =
        """
let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) =
    ()
"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (PatLongIdent (_, s, ps, _), _) ->
            CountAstNode.FunctionSignature(s, ps, None)
            |> isASTLongerThan 65
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result


[<Test>]
let ``count record instance`` () =
    let ast =
        """let a =
            { Type = "SynModuleDecl.ModuleAbbrev"
              Range = r range
              Properties = p ["ident" ==> i ident; "longIdent" ==> li longIdent]
              FsAstNode = ast
              Childs = [] }"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (_, Record (inheritOpt, xs, eo)) ->
            CountAstNode.RecordInstance(inheritOpt, xs, eo)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result

[<Test>]
let ``count anon record instance`` () =
    let ast =
        """let a =
                {| name = getMonthName m
                       month = m
                       balance = Projections.calculateBalance m y events |}"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (_, AnonRecord (_, fields, copyInfo)) ->
            CountAstNode.AnonRecordInstance(fields, copyInfo)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result

[<Test>]
let ``count ArrayOrListOfSeqExpr`` () =
    let ast =
        """
let a2 =
    [|
        for n in 1 .. 100 do
            if isPrime n then yield n
    |]"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (_, ArrayOrListOfSeqExpr (_, e)) ->
            CountAstNode.SyntaxExpression(e)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if not result
    then pass ()
    else failwithf "expected not to be higher than threshold"

[<Test>]
let ``count infix operation`` () =
    let ast =
        """
let webApp =
    GET >=> route "/user"     >=> mustBeLoggedIn >=> userHandler
"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (_, InfixApps (e, es)) ->
            CountAstNode.InfixExpression(e, es)
            |> isASTLongerThan 30
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result

[<Test>]
let ``count record type definition`` () =
    let ast =
        """
type ASTContext =
    {
      /// Original file name without extension of the parsed AST
      TopLevelModuleName: string
      /// Current node is the first child of its parent
      IsFirstChild: bool
      /// Current node is a subnode deep down in an interface
      InterfaceRange: range option
      /// This pattern matters for formatting extern declarations
      IsCStylePattern: bool
      /// Range operators are naked in 'for..in..do' constructs
      IsNakedRange: bool
      /// The optional `|` in pattern matching and union type definitions
      HasVerticalBar: bool
      /// A field is rendered as union field or not
      IsUnionField: bool
      /// First type param might need extra spaces to avoid parsing errors on `<^`, `<'`, etc.
      IsFirstTypeParam: bool
      /// Check whether the context is inside DotGet to suppress whitespaces
      IsInsideDotGet: bool
      /// Check whether the context is inside a SynMemberDefn.Member(memberDefn,range)
      /// This is required to correctly detect the setting SpaceBeforeMember
      IsMemberDefinition: bool
    }
"""
        |> getAst

    let result =
        match ast with
        | SingleModuleDeclarationInFile (Types ([ TypeDef (_, _, _, _, _, Simple (TDSRRecord (_, fs)), _, _, _) ])) ->
            CountAstNode.RecordTypeDefinition(fs)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result

[<Test>]
let ``count anon record type`` () =
    let ast =
        """
type a = {| foo : string; bar : string |}
"""
        |> getAst

    let result =
        match ast with
        | SingleModuleDeclarationInFile (Types ([ TypeDef (_,
                                                           _,
                                                           _,
                                                           _,
                                                           _,
                                                           Simple (TDSRTypeAbbrev (TAnonRecord (_, fields))),
                                                           _,
                                                           _,
                                                           _) ])) ->
            CountAstNode.AnonRecordTypeDefinition(fields)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if not result
    then pass ()
    else failwithf "expected not to be higher than threshold"

[<Test>]
let ``count elmish element`` () =
    let ast =
        """
let view dispatch model =
    div [| Class "container" |] [
        h1 [] [| str "my title" |]
        button [| OnClick(fun _ -> dispatch Msg.Foo) |] [
            str "click me"
        ]
    ]
"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (_, ElmishReactWithChildren ((identifier, _, _), attributes, (_, children))) ->
            CountAstNode.ElmishElement(identifier, attributes, children)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result

[<Test>]
let ``count array`` () =
    let ast =
        """
let a =
    [| one; two; three; four; five; six; seven; eight; nine; ten; eleven |]
"""
        |> getAst

    let result =
        match ast with
        | SingleLetBindingInFile (_, ArrayOrList (_, xs, _)) ->
            CountAstNode.MultipleSyntaxExpressions(xs)
            |> isASTLongerThan 40
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result
