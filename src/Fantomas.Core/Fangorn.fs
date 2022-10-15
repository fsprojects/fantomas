module Fantomas.Core.Fangorn

open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SyntaxOak

type SynIdent with

    member x.ToNode() =
        let (SynIdent (ident, _trivia)) = x
        IdentNode(ident.idText, ident.idRange)

let mkIdentListNodeFromLongIdent (longIdent: LongIdent) =
    match longIdent with
    | [] -> []
    | [ single ] -> [ IdentifierOrDot.Ident(IdentNode(single.idText, single.idRange)) ]
    | head :: tail ->
        let rest =
            tail
            |> List.collect (fun ident ->
                [ IdentifierOrDot.UnknownDot
                  IdentifierOrDot.Ident(IdentNode(ident.idText, ident.idRange)) ])

        IdentifierOrDot.Ident(IdentNode(head.idText, head.idRange)) :: rest

let parseExpressionInSynBinding returnInfo expr =
    match returnInfo, expr with
    | Some (SynBindingReturnInfo (typeName = t1)), SynExpr.Typed (e, t2, _) when RangeHelpers.rangeEq t1.Range t2.Range ->
        e
    | _ -> expr

let mkExpr (sourceText: ISourceText) (e: SynExpr) =
    match e with
    | SynExpr.Const (SynConst.Int32 _, _) ->
        let number = sourceText.GetContentAt(e.Range)
        IdentNode(number, e.Range) |> Expr.Constant
    | _ -> failwith "todo, 693F570D-5A08-4E44-8937-FF98CE0AD8FC"

let mkBinding
    (sourceText: ISourceText)
    (SynBinding (_ao, _, _isInline, _isMutable, _attrs, _px, _, pat, returnInfo, expr, _, _, trivia))
    =
    let leadingKeyword =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.Let m -> IdentNode("let", m)
        | _ -> failwith "todo, FF881966-836F-4425-A600-8C928DE4CDE1"

    let functionName =
        match pat with
        | SynPat.Named (ident = ident) -> ident.ToNode()
        | _ -> failwith "todo, 92E86FB8-1927-4CCD-AF73-244BD6327EB1"

    let equals = SingleTokenNode("=", trivia.EqualsRange.Value)

    let e = parseExpressionInSynBinding returnInfo expr
    let _rt = Option.map (fun (SynBindingReturnInfo (typeName = t)) -> t) returnInfo

    let range =
        let start =
            // if not xmlDoc.IsEmpty then
            //     xmlDoc.Range
            // elif not attributes.IsEmpty then
            //     attributes.Head.Range
            // else
            match trivia.LeadingKeyword, pat with
            | SynLeadingKeyword.Member _, SynPat.LongIdent(extraId = Some _) -> pat.Range
            | _ -> trivia.LeadingKeyword.Range

        unionRanges start e.Range

    Binding(leadingKeyword, functionName, Seq.empty, equals, (mkExpr sourceText expr), range)

let mkModuleDecl (sourceText: ISourceText) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding sourceText singleBinding |> ModuleDecl.TopLevelBinding
    | _ -> failwith "todo, 068F312B-A840-4E14-AF82-A000652532E8"

let mkModuleOrNamespace
    (sourceText: ISourceText)
    (SynModuleOrNamespace (longId = longId; kind = kind; decls = decls; range = range; trivia = trivia))
    =
    let leadingKeyword =
        Option.map (fun mk -> IdentNode("module", mk)) trivia.ModuleKeyword

    let name =
        match kind with
        | SynModuleOrNamespaceKind.AnonModule -> IdentListNode.Empty
        | _ -> IdentListNode(mkIdentListNodeFromLongIdent longId)

    let decls = List.map (mkModuleDecl sourceText) decls

    ModuleOrNamespace(leadingKeyword, name, decls, range)

let mkImplFile (sourceText: ISourceText) (ParsedImplFileInput (contents = contents)) =
    Oak(List.map (mkModuleOrNamespace sourceText) contents)

let mkOak (sourceText: ISourceText) (ast: ParsedInput) =
    match ast with
    | ParsedInput.ImplFile parsedImplFileInput -> mkImplFile sourceText parsedImplFileInput
    | ParsedInput.SigFile _ -> failwith "todo 75E74A3A-C84D-4150-8D49-F111F0916839"
