module Fantomas.Analyzers.AnnotationAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-ANNOTATE-001"

[<Literal>]
let Name: string = "AnnotationAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a let binding without a type annotation, where a written type would say what the name holds."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-annotate-001"

// Whether a pattern carries a type, looking through the wrappers that can sit between the pattern
// and its annotation.
//
// A tuple parameter is typed when every element of it is. `(a: int, b: string)` states the type of
// the parameter just as fully as `((a, b): int * string)` does, and it is the spelling anyone
// writes, so asking for the second would be asking for a worse line.
let rec isTyped (pattern: SynPat) : bool =
    match pattern with
    | SynPat.Typed _ -> true
    | SynPat.Paren(pat = inner) -> isTyped inner
    | SynPat.Attrib(pat = inner) -> isTyped inner
    | SynPat.Tuple(elementPats = elements) -> not (List.isEmpty elements) && List.forall isTyped elements
    | _ -> false

// Whether a parameter is one there is no way to annotate, which is the unit argument and nothing
// else. `let f () : int` has no place to put a type, so asking for one would flag every function
// that takes no argument.
let rec isUnit (pattern: SynPat) : bool =
    match pattern with
    | SynPat.Paren(pat = inner) -> isUnit inner
    | SynPat.Const(constant = SynConst.Unit) -> true
    | _ -> false

// Whether a binding is a `let`, as opposed to a member or anything else that also parses to a
// `SynBinding`.
let isLetBinding (keyword: SynLeadingKeyword) : bool =
    match keyword with
    | SynLeadingKeyword.Let _
    | SynLeadingKeyword.LetRec _
    | SynLeadingKeyword.And _
    | SynLeadingKeyword.StaticLet _
    | SynLeadingKeyword.StaticLetRec _ -> true
    | _ -> false

// What a single binding is missing, if anything, reported on the name rather than on the whole
// binding so that the range stays small.
//
// A value binding carries its type in `returnInfo` rather than on the pattern, so `let x: int = 1`
// and `let f (a: int) : int = a` are both answered by the same field. A function binding needs its
// parameters looked at as well. Anything else, a tuple or a record pattern on the left of the
// equals, has no sensible annotation to ask for and is passed over.
let missingAnnotations (binding: SynBinding) : (range * string) list =
    match binding with
    | SynBinding(headPat = headPattern; returnInfo = returnInfo) ->
        let hasReturnType: bool = Option.isSome returnInfo

        match headPattern with
        | SynPat.Typed _ -> []
        | SynPat.Named(ident = SynIdent(ident = name)) ->
            if hasReturnType then
                []
            else
                [ name.idRange, $"`%s{name.idText}` has no type annotation." ]
        | SynPat.LongIdent(longDotId = SynLongIdent(id = identifiers); argPats = SynArgPats.Pats parameters) ->
            let name: string =
                identifiers
                |> List.tryLast
                |> Option.map (fun (i: Ident) -> i.idText)
                |> Option.defaultValue "this binding"

            let untypedParameters: (range * string) list =
                parameters
                |> List.choose (fun (parameter: SynPat) ->
                    if isTyped parameter || isUnit parameter then
                        None
                    else
                        Some(parameter.Range, $"A parameter of `%s{name}` has no type annotation.")
                )

            let missingReturn: (range * string) list =
                if hasReturnType then
                    []
                else
                    // An uppercase name parses as a `LongIdent` rather than a `Named`, because it
                    // could be a union case, so a value can arrive here with no parameters at all.
                    // It is still a value, and asking it for a return type would read oddly.
                    let what: string =
                        match parameters with
                        | [] -> "type"
                        | _ -> "return type"

                    match List.tryLast identifiers with
                    | None -> []
                    | Some last -> [ last.idRange, $"`%s{name}` has no %s{what} annotation." ]

            untypedParameters @ missingReturn
        | _ -> []

// Every let binding that is missing a type, with the test bindings passed over.
//
// Signature files are skipped whole: a `val` already states the type, which is the point.
let analyze (parsedInput: ParsedInput) : Message list =
    match parsedInput with
    | ParsedInput.SigFile _ -> []
    | ParsedInput.ImplFile _ ->

        let findings: ResizeArray<range * string> = ResizeArray<range * string>()
        let exempt: ResizeArray<range> = ResizeArray<range>()

        let walker: SyntaxCollectorBase =
            { new SyntaxCollectorBase() with
                override _.WalkBinding(_path: SyntaxVisitorPath, binding: SynBinding) : unit =
                    match binding with
                    | SynBinding(attributes = attributes; trivia = { LeadingKeyword = keyword }) ->
                        if isTest attributes then
                            // The body has to be part of this, so that the locals inside a test are
                            // exempt too. `SynBinding.range` stops before the right hand side.
                            exempt.Add binding.RangeOfBindingWithRhs
                        elif isLetBinding keyword then
                            findings.AddRange(missingAnnotations binding)
            }

        walkAst walker parsedInput

        // A test body is scaffolding, so the locals inside one are passed over along with the test
        // itself. The exempt ranges are only known once the whole file has been walked, which is why
        // this filters at the end rather than while collecting.
        findings
        |> Seq.choose (fun (name: range, text: string) ->
            if Seq.exists (fun (test: range) -> Range.rangeContainsRange test name) exempt then
                None
            else
                Some
                    {
                        Type = Name
                        Message = text
                        Code = Code
                        Severity = Severity.Warning
                        Range = name
                        Fixes = []
                    }
        )
        |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }
