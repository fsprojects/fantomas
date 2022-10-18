module rec Fantomas.Core.Fangorn

open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.RangePatterns
open Fantomas.Core.SyntaxOak

type TextFromSource = string -> range -> string

type Ident with

    member ident.ToNode() =
        let width = ident.idRange.EndColumn - ident.idRange.StartColumn

        let text =
            if ident.idText.Length + 4 = width then
                // add backticks
                $"``{ident.idText}``"
            else
                ident.idText

        SingleTextNode(text, ident.idRange)

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

let mkConstant (fromSource: TextFromSource) c r : Constant =
    let orElse fallback =
        SingleTextNode(fromSource fallback r, r) |> Constant.FromText

    match c with
    | SynConst.Unit ->
        match r with
        | StartEndRange 1 (lpr, _, rpr) ->
            UnitNode(SingleTextNode("(", lpr), SingleTextNode(")", rpr), r) |> Constant.Unit
    | SynConst.Bool b -> SingleTextNode((if b then "true" else "false"), r) |> Constant.FromText
    | SynConst.Byte v -> orElse $"%A{v}"
    | SynConst.SByte v -> orElse $"%A{v}"
    | SynConst.Int16 v -> orElse $"%A{v}"
    | SynConst.Int32 v -> orElse $"%A{v}"
    | SynConst.Int64 v -> orElse $"%A{v}"
    | SynConst.UInt16 v -> orElse $"%A{v}"
    | SynConst.UInt16s v -> orElse $"%A{v}"
    | SynConst.UInt32 v -> orElse $"%A{v}"
    | SynConst.UInt64 v -> orElse $"%A{v}"
    | SynConst.Double v -> orElse $"%A{v}"
    | SynConst.Single v -> orElse $"%A{v}"
    | SynConst.Decimal v -> orElse $"%A{v}"
    | SynConst.IntPtr v -> orElse $"%A{v}"
    | SynConst.UIntPtr v -> orElse $"%A{v}"
    | SynConst.UserNum _ -> failwith "todo, 90D57090-9123-4344-9B4F-9B51BB50DA31"
    | SynConst.String (s, kind, r) -> failwith "todo, C1CDBFC9-1B5D-471C-8189-CEC3A676D386"
    | SynConst.Char c -> failwith "todo, 9AD2DFA7-80E2-43C7-A573-777987EA941B"
    | SynConst.Bytes (bytes, _, r) -> failwith "todo, ED679198-BED9-42FD-BE24-7E7AD959CE93"
    | SynConst.Measure (c, numberRange, m) -> failwith "todo, 1BF1C723-1931-40BE-8C02-3A4BAC1D8BAD"
    | SynConst.SourceIdentifier (c, _, r) -> SingleTextNode(c, r) |> Constant.FromText

let rec mkExpr (fromSource: TextFromSource) (e: SynExpr) : Expr =
    let exprRange = e.Range

    match e with
    | SynExpr.Lazy (e, StartRange 4 (lazyKeyword, _range)) ->
        ExprLazyNode(SingleTextNode("lazy", lazyKeyword), mkExpr fromSource e, exprRange)
        |> Expr.Lazy
    | SynExpr.InferredDowncast (e, StartRange 8 (downcastKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("downcast", downcastKeyword), false, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.InferredUpcast (e, StartRange 6 (upcastKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("upcast", upcastKeyword), false, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.Assert (e, StartRange 6 (assertKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("assert", assertKeyword), false, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.AddressOf (true, e, _, StartRange 1 (ampersandToken, _range)) ->
        ExprSingleNode(SingleTextNode("&", ampersandToken), false, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.AddressOf (false, e, _, StartRange 2 (ampersandToken, _range)) ->
        ExprSingleNode(SingleTextNode("&&", ampersandToken), false, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturn ((true, _), e, StartRange 5 (yieldKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("yield", yieldKeyword), true, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturn ((false, _), e, StartRange 6 (returnKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("return", returnKeyword), true, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturnFrom ((true, _), e, StartRange 6 (yieldBangKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("yield!", yieldBangKeyword), true, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturnFrom ((false, _), e, StartRange 7 (returnBangKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("return!", returnBangKeyword), true, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.Do (e, StartRange 2 (doKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("do", doKeyword), true, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.DoBang (e, StartRange 3 (doBangKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("do!", doBangKeyword), true, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.Fixed (e, StartRange 5 (fixedKeyword, _range)) ->
        ExprSingleNode(SingleTextNode("fixed", fixedKeyword), false, mkExpr fromSource e, exprRange)
        |> Expr.Single
    | SynExpr.Const (c, r) -> mkConstant fromSource c r |> Expr.Constant
    | SynExpr.Null _ -> SingleTextNode("null", exprRange) |> Expr.Null
    | SynExpr.Quote (_, isRaw, e, _, range) ->
        let startToken, endToken =
            let sText, length, eText = if isRaw then "<@@", 3, "@@>" else "<@", 2, "@>"

            match range with
            | StartEndRange length (startRange, _, endRange) ->
                SingleTextNode(sText, startRange), SingleTextNode(eText, endRange)

        ExprQuoteNode(startToken, mkExpr fromSource e, endToken, exprRange)
        |> Expr.Quote
    | SynExpr.TypeTest (e, t, _) ->
        ExprTypedNode(mkExpr fromSource e, ":?", mkType fromSource t, exprRange)
        |> Expr.Typed
    | SynExpr.Downcast (e, t, _) ->
        ExprTypedNode(mkExpr fromSource e, ":?>", mkType fromSource t, exprRange)
        |> Expr.Typed
    | SynExpr.Upcast (e, t, _) ->
        ExprTypedNode(mkExpr fromSource e, ":>", mkType fromSource t, exprRange)
        |> Expr.Typed
    | SynExpr.Typed (e, t, _) ->
        ExprTypedNode(mkExpr fromSource e, ":", mkType fromSource t, exprRange)
        |> Expr.Typed
    | SynExpr.New (_, t, (SynExpr.Paren _ as px), StartRange 3 (newRange, _))
    | SynExpr.New (_, t, (SynExpr.Const (SynConst.Unit, _) as px), StartRange 3 (newRange, _)) ->
        ExprNewParenNode(SingleTextNode("new", newRange), mkType fromSource t, mkExpr fromSource px, exprRange)
        |> Expr.NewParen
    // | Expr.New _ -> failwith "Not Implemented"
    // | Expr.Tuple _ -> failwith "Not Implemented"
    // | Expr.StructTuple _ -> failwith "Not Implemented"
    // | Expr.ArrayOrList _ -> failwith "Not Implemented"
    // | Expr.Record _ -> failwith "Not Implemented"
    // | Expr.AnonRecord _ -> failwith "Not Implemented"
    // | Expr.ObjExpr _ -> failwith "Not Implemented"
    // | Expr.While _ -> failwith "Not Implemented"
    // | Expr.For _ -> failwith "Not Implemented"
    // | Expr.ForEach _ -> failwith "Not Implemented"
    // | Expr.NamedComputation _ -> failwith "Not Implemented"
    // | Expr.Computation _ -> failwith "Not Implemented"
    // | Expr.CompExprBody _ -> failwith "Not Implemented"
    // | Expr.JoinIn _ -> failwith "Not Implemented"
    // | Expr.ParenLambda _ -> failwith "Not Implemented"
    // | Expr.Lambda _ -> failwith "Not Implemented"
    // | Expr.MatchLambda _ -> failwith "Not Implemented"
    // | Expr.Match _ -> failwith "Not Implemented"
    // | Expr.TraitCall _ -> failwith "Not Implemented"
    // | Expr.ParenILEmbedded _ -> failwith "Not Implemented"
    // | Expr.ParenFunctionNameWithStar _ -> failwith "Not Implemented"
    | SynExpr.Paren (e, lpr, Some rpr, _) ->
        ExprParenNode(SingleTextNode("(", lpr), mkExpr fromSource e, SingleTextNode(")", rpr), exprRange)
        |> Expr.Paren
    // | Expr.Paren _ -> failwith "Not Implemented"
    // | Expr.Dynamic _ -> failwith "Not Implemented"
    // | Expr.PrefixApp _ -> failwith "Not Implemented"
    // | Expr.NewlineInfixAppAlwaysMultiline _ -> failwith "Not Implemented"
    // | Expr.NewlineInfixApps _ -> failwith "Not Implemented"
    // | Expr.SameInfixApps _ -> failwith "Not Implemented"
    // | Expr.TernaryApp _ -> failwith "Not Implemented"
    // | Expr.IndexWithoutDot _ -> failwith "Not Implemented"
    // | Expr.AppDotGetTypeApp _ -> failwith "Not Implemented"
    // | Expr.DotGetAppDotGetAppParenLambda _ -> failwith "Not Implemented"
    // | Expr.DotGetAppParen _ -> failwith "Not Implemented"
    // | Expr.DotGetAppWithParenLambda _ -> failwith "Not Implemented"
    // | Expr.DotGetApp _ -> failwith "Not Implemented"
    // | Expr.AppLongIdentAndSingleParenArg _ -> failwith "Not Implemented"
    // | Expr.AppSingleParenArg _ -> failwith "Not Implemented"
    // | Expr.DotGetAppWithLambda _ -> failwith "Not Implemented"
    // | Expr.AppWithLambda _ -> failwith "Not Implemented"
    // | Expr.NestedIndexWithoutDot _ -> failwith "Not Implemented"
    // | Expr.EndsWithDualListApp _ -> failwith "Not Implemented"
    // | Expr.EndsWithSingleListApp _ -> failwith "Not Implemented"
    // | Expr.App _ -> failwith "Not Implemented"
    // | Expr.TypeApp _ -> failwith "Not Implemented"
    // | Expr.LetOrUses _ -> failwith "Not Implemented"
    // | Expr.TryWithSingleClause _ -> failwith "Not Implemented"
    // | Expr.TryWith _ -> failwith "Not Implemented"
    // | Expr.TryFinally _ -> failwith "Not Implemented"
    // | Expr.Sequentials _ -> failwith "Not Implemented"
    // | Expr.IfThen _ -> failwith "Not Implemented"
    // | Expr.IfThenElse _ -> failwith "Not Implemented"
    // | Expr.IfThenElif _ -> failwith "Not Implemented"
    | SynExpr.Ident ident -> ident.ToNode() |> Expr.Ident
    // | Expr.OptVar _ -> failwith "Not Implemented"
    // | Expr.LongIdentSet _ -> failwith "Not Implemented"
    // | Expr.DotIndexedGet _ -> failwith "Not Implemented"
    // | Expr.DotIndexedSet _ -> failwith "Not Implemented"
    // | Expr.NamedIndexedPropertySet _ -> failwith "Not Implemented"
    // | Expr.DotNamedIndexedPropertySet _ -> failwith "Not Implemented"
    // | Expr.DotGet _ -> failwith "Not Implemented"
    // | Expr.DotSet _ -> failwith "Not Implemented"
    // | Expr.Set _ -> failwith "Not Implemented"
    // | Expr.LibraryOnlyStaticOptimization _ -> failwith "Not Implemented"
    // | Expr.InterpolatedStringExpr _ -> failwith "Not Implemented"
    // | Expr.IndexRangeWildcard _ -> failwith "Not Implemented"
    // | Expr.IndexRange _ -> failwith "Not Implemented"
    // | Expr.IndexFromEnd _ -> failwith "Not Implemented"
    // | Expr.Typar _ -> failwith "Not Implemented"
    | _ -> failwith "todo, 693F570D-5A08-4E44-8937-FF98CE0AD8FC"

let mkPat (fromSource: TextFromSource) (p: SynPat) =
    let patternRange = p.Range

    match p with
    // | Pattern.OptionalVal _ -> failwith "Not Implemented"
    // | Pattern.Attrib _ -> failwith "Not Implemented"
    // | Pattern.Or _ -> failwith "Not Implemented"
    // | Pattern.Ands _ -> failwith "Not Implemented"
    | SynPat.Null _ -> SingleTextNode("null", patternRange) |> Pattern.Null
    | SynPat.Wild _ -> SingleTextNode("_", patternRange) |> Pattern.Wild
    // | Pattern.Typed _ -> failwith "Not Implemented"
    | SynPat.Named (ident = ident) -> PatNamedNode(ident.ToNode(), patternRange) |> Pattern.Named
    // | Pattern.As _ -> failwith "Not Implemented"
    // | Pattern.ListCons _ -> failwith "Not Implemented"
    // | Pattern.NamePatPairs _ -> failwith "Not Implemented"
    // | Pattern.LongIdentParen _ -> failwith "Not Implemented"
    // | Pattern.LongIdent _ -> failwith "Not Implemented"
    | SynPat.Paren (SynPat.Const (SynConst.Unit, _), StartEndRange 1 (lpr, _, rpr)) ->
        UnitNode(SingleTextNode("(", lpr), SingleTextNode(")", rpr), patternRange)
        |> Pattern.Unit
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
        | SynPat.Named (ident = ident) -> Choice1Of2(ident.ToNode()), []
        | SynPat.LongIdent (longDotId = SynLongIdent ([ _ ], _, _) as lid; argPats = SynArgPats.Pats ps) ->
            Choice1Of2(lid.IdentsWithTrivia.[0].ToNode()), List.map (mkPat fromSource) ps
        | _ -> Choice2Of2(mkPat fromSource pat), []

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
    // | OpenList of OpenListNode
    // | HashDirectiveList of HashDirectiveListNode
    // | AttributesList of AttributesListNode
    | SynModuleDecl.Expr (e, _) -> mkExpr fromSource e |> ModuleDecl.DeclExpr
    // | ExternBinding of ExternBindingNode
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding fromSource singleBinding |> ModuleDecl.TopLevelBinding
    // | ModuleAbbrev of ModuleAbbrevNode
    // | NestedModule of ModuleOrNamespaceNode
    // | TypeDefn of TypeDefn
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
                unionRanges tk (identifierNode :> Node).Range
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
