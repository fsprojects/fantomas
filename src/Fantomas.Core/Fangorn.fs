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
        SingleTextNode(ident.idText, ident.idRange)

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
    | [ single ] ->
        IdentListNode([ IdentifierOrDot.Ident(SingleTextNode(single.idText, single.idRange)) ], single.idRange)
    | head :: tail ->
        let rest =
            tail
            |> List.collect (fun ident ->
                [ IdentifierOrDot.UnknownDot
                  IdentifierOrDot.Ident(SingleTextNode(ident.idText, ident.idRange)) ])

        let range =
            longIdent |> List.map (fun ident -> ident.idRange) |> List.reduce unionRanges

        IdentListNode(IdentifierOrDot.Ident(SingleTextNode(head.idText, head.idRange)) :: rest, range)

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

                SingleTextNode(fromSource fallback range, range)
            | ParsedHashDirectiveArgument.SourceIdentifier (identifier, _, range) -> SingleTextNode(identifier, range))

    ParsedHashDirectiveNode(ident, args, range)

let mkExpr (fromSource: TextFromSource) (e: SynExpr) =
    match e with
    | SynExpr.Const (SynConst.Int32 i32, _) ->
        let number = fromSource (string i32) e.Range
        SingleTextNode(number, e.Range) |> Expr.Constant
    | _ -> failwith "todo, 693F570D-5A08-4E44-8937-FF98CE0AD8FC"

let mkPat (fromSource: TextFromSource) (p: SynPat) =
    match p with
    // | Pattern.OptionalVal _ -> failwith "Not Implemented"
    // | Pattern.Attrib _ -> failwith "Not Implemented"
    // | Pattern.Or _ -> failwith "Not Implemented"
    // | Pattern.Ands _ -> failwith "Not Implemented"
    // | Pattern.Null _ -> failwith "Not Implemented"
    // | Pattern.Wild _ -> failwith "Not Implemented"
    // | Pattern.Typed _ -> failwith "Not Implemented"
    | SynPat.Named (ident = ident) -> PatNamedNode(ident.ToNode(), p.Range) |> Pattern.Named
    // | Pattern.As _ -> failwith "Not Implemented"
    // | Pattern.ListCons _ -> failwith "Not Implemented"
    // | Pattern.NamePatPairs _ -> failwith "Not Implemented"
    // | Pattern.LongIdentParen _ -> failwith "Not Implemented"
    // | Pattern.LongIdent _ -> failwith "Not Implemented"
    // | Pattern.Unit _ -> failwith "Not Implemented"
    // | Pattern.Paren _ -> failwith "Not Implemented"
    // | Pattern.Tuple _ -> failwith "Not Implemented"
    // | Pattern.StructTuple _ -> failwith "Not Implemented"
    // | Pattern.ArrayOrList _ -> failwith "Not Implemented"
    // | Pattern.Record _ -> failwith "Not Implemented"
    // | Pattern.Const _ -> failwith "Not Implemented"
    // | Pattern.IsInst _ -> failwith "Not Implemented"
    // | Pattern.QuoteExpr _ -> failwith "Not Implemented"
    | _ -> failwith "todo, 52DBA54F-37FE-45F1-9DDC-7BF7DE2F3502"

let mkBinding
    (fromSource: TextFromSource)
    (SynBinding (_ao, _, _isInline, _isMutable, _attrs, _px, _, pat, returnInfo, expr, _, _, trivia))
    =
    let leadingKeyword =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.Let m -> SingleTextNode("let", m)
        | _ -> failwith "todo, FF881966-836F-4425-A600-8C928DE4CDE1"

    let functionName, parameters =
        match pat with
        | SynPat.Named (ident = ident) -> ident.ToNode(), []
        | SynPat.LongIdent (longDotId = (SynLongIdent ([ _ ], _, _) as lid); argPats = SynArgPats.Pats ps) ->
            lid.IdentsWithTrivia.[0].ToNode(), List.map (mkPat fromSource) ps
        | _ -> failwith "todo, 92E86FB8-1927-4CCD-AF73-244BD6327EB1"

    let equals = SingleTextNode("=", trivia.EqualsRange.Value)

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

    BindingNode(leadingKeyword, functionName, parameters, equals, (mkExpr fromSource expr), range)

let mkModuleDecl (fromSource: TextFromSource) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding fromSource singleBinding |> ModuleDecl.TopLevelBinding
    | _ -> failwith "todo, 068F312B-A840-4E14-AF82-A000652532E8"

let mkType (fromSource: TextFromSource) (t: SynType) : Type =
    match t with
    // | Funs of TypeFunsNode
    // | Tuple of TypeTupleNode
    // | HashConstraint of TypeHashConstraintNode
    // | MeasurePower of TypeMeasurePowerNode
    // | MeasureDivide of TypeMeasureDivideNode
    // | StaticConstant of TypeStaticConstantNode
    // | StaticConstantExpr of TypeStaticConstantExprNode
    // | StaticConstantNamed of TypeStaticConstantNamedNode
    // | Array of TypeArrayNode
    // | Anon of TypeAnonNode
    // | Var of TypeVarNode
    // | App of TypeAppNode
    // | LongIdentApp of TypeLongIdentAppNode
    // | StructTuple of TypeStructTupleNode
    // | WithGlobalConstraints of TypeWithGlobalConstraintsNode
    | SynType.LongIdent lid -> Type.LongIdent(lid.ToNode())
    // | AnonRecord of TypeAnonRecordNode
    // | Paren of TypeParenNode
    // | SignatureParameter of TypeSignatureParameterNode
    // | Or of TypeOrNode
    | _ -> failwith "todo, F28E0FA1-7C39-4BFF-AFBF-0E9FD3D1D4E4"

let rec (|OpenL|_|) =
    function
    | SynModuleDecl.Open (target, range) :: OpenL (xs, ys) -> Some((target, range) :: xs, ys)
    | SynModuleDecl.Open (target, range) :: ys -> Some([ target, range ], ys)
    | _ -> None

let mkOpenNodeForImpl (fromSource: TextFromSource) (target, range) : Open =
    match target with
    | SynOpenDeclTarget.ModuleOrNamespace (longId, _) ->
        OpenModuleOrNamespaceNode(longId.ToNode(), range) |> Open.ModuleOrNamespace
    | SynOpenDeclTarget.Type (typeName, range) -> OpenTargetNode(mkType fromSource typeName, range) |> Open.Target

let mkTypeDefn
    (fromSource: TextFromSource)
    (isFirst: bool)
    (SynTypeDefn (typeInfo, typeRepr, members, implicitConstructor, range, trivia))
    : TypeDefn =

    let typeNameNode =
        match typeInfo, trivia.TypeKeyword with
        | SynComponentInfo (ats, tds, tcs, lid, px, preferPostfix, ao, _), Some tk ->
            let identifierNode = mkIdentListNodeFromLongIdent lid

            TypeNameNode(
                AttributesListNode.Empty,
                SingleTextNode((if isFirst then "type" else "and"), tk),
                isFirst,
                None,
                identifierNode,
                None,
                Option.map (fun eq -> SingleTextNode("=", eq)) trivia.EqualsRange,
                None,
                unionRanges tk identifierNode.Range
            )
        | _, None ->
            // TODO: update dotnet/fsharp to add "and" keywords.
            failwith "leading keyword should be present"

    match typeRepr with
    // | Simple (TDSREnum ecs) ->
    // | Simple (TDSRUnion (ao', xs)) ->
    // | Simple (TDSRRecord (openingBrace, ao', fs, closingBrace)) ->
    // | Simple TDSRNone -> typeName
    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev (rhsType = t)) ->
        TypeDefn.Abbrev(TypeDefnAbbrevNode(typeNameNode, mkType fromSource t, range))
    // | Simple (TDSRException (ExceptionDefRepr (ats, px, ao, uc)))
    // | ObjectModel (TCSimple (TCInterface | TCClass) as tdk, MemberDefnList (impCtor, others), range) ->
    // | ObjectModel (TCSimple TCStruct as tdk, MemberDefnList (impCtor, others), _) ->
    // | ObjectModel (TCSimple (TCAugmentation withKeywordAug), _, _) ->
    // | ObjectModel (TCDelegate (FunType ts), _, _) ->
    // | ObjectModel (TCSimple TCUnspecified, MemberDefnList (impCtor, others), _) when not (List.isEmpty ms) ->
    // | ObjectModel (_, MemberDefnList (impCtor, others), _) ->
    // | ExceptionRepr (ExceptionDefRepr (ats, px, ao, uc)) -> genExceptionBody ats px ao uc
    | _ -> failwith "not implemented, C8C6C667-6A67-46A6-9EE3-A0DF663A3A91"

let rec mkModuleDecls
    (fromSource: TextFromSource)
    (decls: SynModuleDecl list)
    (finalContinuation: ModuleDecl list -> ModuleDecl list)
    =
    match decls with
    | [] -> finalContinuation []
    | OpenL (xs, ys) ->
        let openListNode =
            List.map (mkOpenNodeForImpl fromSource) xs
            |> OpenListNode
            |> ModuleDecl.OpenList

        mkModuleDecls fromSource ys (fun nodes -> openListNode :: nodes)

    | SynModuleDecl.Types (typeDefns = typeDefns) :: rest ->
        let typeNodes =
            List.mapi (fun idx tdn -> mkTypeDefn fromSource (idx = 0) tdn |> ModuleDecl.TypeDefn) typeDefns

        mkModuleDecls fromSource rest (fun nodes -> [ yield! typeNodes; yield! nodes ])
    | head :: tail ->
        mkModuleDecls fromSource tail (fun nodes -> mkModuleDecl fromSource head :: nodes |> finalContinuation)

let mkModuleOrNamespace
    (fromSource: TextFromSource)
    (SynModuleOrNamespace (longId = longId; kind = kind; decls = decls; range = range; trivia = trivia))
    =
    let leadingKeyword =
        match trivia.ModuleKeyword with
        | Some moduleKeyword -> Some(SingleTextNode("module", moduleKeyword))
        | None ->
            trivia.NamespaceKeyword
            |> Option.map (fun mk -> SingleTextNode("namespace", mk))

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
