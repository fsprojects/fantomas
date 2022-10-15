module Fantomas.Core.Fangorn

open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SyntaxOak

type TextFromSource = string -> range -> string

type SynIdent with

    member x.ToNode() =
        let (SynIdent (ident, _trivia)) = x
        IdentNode(ident.idText, ident.idRange)

type SynLongIdent with

    member x.ToNode() =
        match x.IdentsWithTrivia with
        | [] -> IdentListNode.Empty
        | [ single ] -> IdentListNode([ IdentifierOrDot.Ident(single.ToNode()) ], x.Range)
        | head :: tail ->
            assert (tail.Length = x.Dots.Length)

            let rest =
                (x.Dots, tail)
                ||> List.zip
                |> List.collect (fun (dot, ident) ->
                    [ IdentifierOrDot.KnownDot(DotNode(dot))
                      IdentifierOrDot.Ident(ident.ToNode()) ])

            IdentListNode(IdentifierOrDot.Ident(head.ToNode()) :: rest, x.Range)

let mkIdentListNodeFromLongIdent (longIdent: LongIdent) : IdentListNode =
    match longIdent with
    | [] -> IdentListNode.Empty
    | [ single ] -> IdentListNode([ IdentifierOrDot.Ident(IdentNode(single.idText, single.idRange)) ], single.idRange)
    | head :: tail ->
        let rest =
            tail
            |> List.collect (fun ident ->
                [ IdentifierOrDot.UnknownDot
                  IdentifierOrDot.Ident(IdentNode(ident.idText, ident.idRange)) ])

        let range =
            longIdent |> List.map (fun ident -> ident.idRange) |> List.reduce unionRanges

        IdentListNode(IdentifierOrDot.Ident(IdentNode(head.idText, head.idRange)) :: rest, range)

let parseExpressionInSynBinding returnInfo expr =
    match returnInfo, expr with
    | Some (SynBindingReturnInfo (typeName = t1)), SynExpr.Typed (e, t2, _) when RangeHelpers.rangeEq t1.Range t2.Range ->
        e
    | _ -> expr

let mkParsedHashDirective (fromSource: TextFromSource) (ParsedHashDirective (ident, args, range)) =
    let args =
        args
        |> List.map (function
            | ParsedHashDirectiveArgument.String (value, stringKind, range) ->
                let fallback =
                    match stringKind with
                    | SynStringKind.Regular -> sprintf "\"%s\"" value
                    | SynStringKind.Verbatim -> sprintf "@\"%s\"" value
                    | SynStringKind.TripleQuote -> sprintf "\"\"\"%s\"\"\"" value

                IdentNode(fromSource fallback range, range)
            | ParsedHashDirectiveArgument.SourceIdentifier (identifier, _, range) -> IdentNode(identifier, range))

    ParsedHashDirectiveNode(ident, args, range)

let mkExpr (fromSource: TextFromSource) (e: SynExpr) =
    match e with
    | SynExpr.Const (SynConst.Int32 i32, _) ->
        let number = fromSource (string i32) e.Range
        IdentNode(number, e.Range) |> Expr.Constant
    | _ -> failwith "todo, 693F570D-5A08-4E44-8937-FF98CE0AD8FC"

let mkBinding
    (fromSource: TextFromSource)
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

    BindingNode(leadingKeyword, functionName, Seq.empty, equals, (mkExpr fromSource expr), range)

let mkModuleDecl (fromSource: TextFromSource) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding fromSource singleBinding |> ModuleDecl.TopLevelBinding
    | _ -> failwith "todo, 068F312B-A840-4E14-AF82-A000652532E8"

let mkType _ =
    failwith "todo, F28E0FA1-7C39-4BFF-AFBF-0E9FD3D1D4E4"

let rec (|OpenL|_|) =
    function
    | SynModuleDecl.Open (target, range) :: OpenL (xs, ys) -> Some((target, range) :: xs, ys)
    | SynModuleDecl.Open (target, range) :: ys -> Some([ target, range ], ys)
    | _ -> None

let mkOpenNodeForImpl (target, range) : Open =
    match target with
    | SynOpenDeclTarget.ModuleOrNamespace (longId, _) ->
        OpenModuleOrNamespaceNode(longId.ToNode(), range) |> Open.ModuleOrNamespace
    | SynOpenDeclTarget.Type (typeName, range) -> OpenTargetNode(mkType typeName, range) |> Open.Target

let rec mkModuleDecls
    (fromSource: TextFromSource)
    (decls: SynModuleDecl list)
    (finalContinuation: ModuleDecl list -> ModuleDecl list)
    =
    match decls with
    | [] -> finalContinuation []
    | OpenL (xs, ys) ->
        let openListNode =
            List.map mkOpenNodeForImpl xs |> OpenListNode |> ModuleDecl.OpenList

        mkModuleDecls fromSource ys (fun nodes -> openListNode :: nodes)
    | head :: tail ->
        mkModuleDecls fromSource tail (fun nodes -> mkModuleDecl fromSource head :: nodes |> finalContinuation)

let mkModuleOrNamespace
    (fromSource: TextFromSource)
    (SynModuleOrNamespace (longId = longId; kind = kind; decls = decls; range = range; trivia = trivia))
    =
    let leadingKeyword =
        match trivia.ModuleKeyword with
        | Some moduleKeyword -> Some(IdentNode("module", moduleKeyword))
        | None -> trivia.NamespaceKeyword |> Option.map (fun mk -> IdentNode("namespace", mk))

    let name =
        match kind with
        | SynModuleOrNamespaceKind.AnonModule -> IdentListNode.Empty
        | _ -> mkIdentListNodeFromLongIdent longId

    let decls = mkModuleDecls fromSource decls id

    ModuleOrNamespaceNode(leadingKeyword, name, decls, range)

let mkImplFile
    (fromSource: TextFromSource)
    (ParsedImplFileInput (hashDirectives = hashDirectives; contents = contents))
    =
    let phds = List.map (mkParsedHashDirective fromSource) hashDirectives
    let mds = List.map (mkModuleOrNamespace fromSource) contents
    Oak(phds, mds)

let mkOak (sourceText: ISourceText option) (ast: ParsedInput) =
    let fromSource fallback range =
        match sourceText with
        | None -> fallback
        | Some sourceText -> sourceText.GetContentAt range

    match ast with
    | ParsedInput.ImplFile parsedImplFileInput -> mkImplFile fromSource parsedImplFileInput
    | ParsedInput.SigFile _ -> failwith "todo 75E74A3A-C84D-4150-8D49-F111F0916839"
