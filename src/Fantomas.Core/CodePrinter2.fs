module internal rec Fantomas.Core.CodePrinter2

open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

let genTrivia (trivia: TriviaNode) (ctx: Context) =
    let currentLastLine = ctx.WriterModel.Lines |> List.tryHead

    // Some items like #if or Newline should be printed on a newline
    // It is hard to always get this right in CodePrinter, so we detect it based on the current code.
    let addNewline =
        currentLastLine
        |> Option.map (fun line -> line.Trim().Length > 0)
        |> Option.defaultValue false

    let addSpace =
        currentLastLine
        |> Option.bind (fun line -> Seq.tryLast line |> Option.map (fun lastChar -> lastChar <> ' '))
        |> Option.defaultValue false

    let gen =
        match trivia.Content with
        | CommentOnSingleLine comment -> (ifElse addNewline sepNlnForTrivia sepNone) +> !-comment +> sepNlnForTrivia
        | Newline -> (ifElse addNewline (sepNlnForTrivia +> sepNlnForTrivia) sepNlnForTrivia)

    gen ctx

let genNode<'n when 'n :> NodeBase> (n: 'n) (f: Context -> Context) =
    col sepNone n.ContentBefore genTrivia
    +> f
    +> col sepNone n.ContentAfter genTrivia

let genSingleTextNode (node: SingleTextNode) = !-node.Text |> genNode node

let genIdentListNode (iln: IdentListNode) =
    col sepNone iln.Content (function
        | IdentifierOrDot.Ident ident -> genSingleTextNode ident
        | IdentifierOrDot.KnownDot _
        | IdentifierOrDot.UnknownDot _ -> sepDot)
    |> genNode iln

let genParsedHashDirective (phd: ParsedHashDirectiveNode) =
    !- "#" +> !-phd.Ident +> sepSpace +> col sepSpace phd.Args genSingleTextNode
    |> genNode phd

let genExpr (e: Expr) =
    match e with
    | Expr.Lazy _ -> failwith "Not Implemented"
    | Expr.Single _ -> failwith "Not Implemented"
    | Expr.Constant node -> genSingleTextNode node
    | Expr.Null _ -> failwith "Not Implemented"
    | Expr.Quote _ -> failwith "Not Implemented"
    | Expr.Typed _ -> failwith "Not Implemented"
    | Expr.New _ -> failwith "Not Implemented"
    | Expr.Tuple _ -> failwith "Not Implemented"
    | Expr.StructTuple _ -> failwith "Not Implemented"
    | Expr.ArrayOrList _ -> failwith "Not Implemented"
    | Expr.Record _ -> failwith "Not Implemented"
    | Expr.AnonRecord _ -> failwith "Not Implemented"
    | Expr.ObjExpr _ -> failwith "Not Implemented"
    | Expr.While _ -> failwith "Not Implemented"
    | Expr.For _ -> failwith "Not Implemented"
    | Expr.ForEach _ -> failwith "Not Implemented"
    | Expr.NamedComputation _ -> failwith "Not Implemented"
    | Expr.Computation _ -> failwith "Not Implemented"
    | Expr.CompExprBody _ -> failwith "Not Implemented"
    | Expr.JoinIn _ -> failwith "Not Implemented"
    | Expr.ParenLambda _ -> failwith "Not Implemented"
    | Expr.Lambda _ -> failwith "Not Implemented"
    | Expr.MatchLambda _ -> failwith "Not Implemented"
    | Expr.Match _ -> failwith "Not Implemented"
    | Expr.TraitCall _ -> failwith "Not Implemented"
    | Expr.ParenILEmbedded _ -> failwith "Not Implemented"
    | Expr.ParenFunctionNameWithStar _ -> failwith "Not Implemented"
    | Expr.Paren _ -> failwith "Not Implemented"
    | Expr.Dynamic _ -> failwith "Not Implemented"
    | Expr.PrefixApp _ -> failwith "Not Implemented"
    | Expr.NewlineInfixAppAlwaysMultiline _ -> failwith "Not Implemented"
    | Expr.NewlineInfixApps _ -> failwith "Not Implemented"
    | Expr.SameInfixApps _ -> failwith "Not Implemented"
    | Expr.TernaryApp _ -> failwith "Not Implemented"
    | Expr.IndexWithoutDot _ -> failwith "Not Implemented"
    | Expr.AppDotGetTypeApp _ -> failwith "Not Implemented"
    | Expr.DotGetAppDotGetAppParenLambda _ -> failwith "Not Implemented"
    | Expr.DotGetAppParen _ -> failwith "Not Implemented"
    | Expr.DotGetAppWithParenLambda _ -> failwith "Not Implemented"
    | Expr.DotGetApp _ -> failwith "Not Implemented"
    | Expr.AppLongIdentAndSingleParenArg _ -> failwith "Not Implemented"
    | Expr.AppSingleParenArg _ -> failwith "Not Implemented"
    | Expr.DotGetAppWithLambda _ -> failwith "Not Implemented"
    | Expr.AppWithLambda _ -> failwith "Not Implemented"
    | Expr.NestedIndexWithoutDot _ -> failwith "Not Implemented"
    | Expr.EndsWithDualListApp _ -> failwith "Not Implemented"
    | Expr.EndsWithSingleListApp _ -> failwith "Not Implemented"
    | Expr.App _ -> failwith "Not Implemented"
    | Expr.TypeApp _ -> failwith "Not Implemented"
    | Expr.LetOrUses _ -> failwith "Not Implemented"
    | Expr.TryWithSingleClause _ -> failwith "Not Implemented"
    | Expr.TryWith _ -> failwith "Not Implemented"
    | Expr.TryFinally _ -> failwith "Not Implemented"
    | Expr.Sequentials _ -> failwith "Not Implemented"
    | Expr.IfThen _ -> failwith "Not Implemented"
    | Expr.IfThenElse _ -> failwith "Not Implemented"
    | Expr.IfThenElif _ -> failwith "Not Implemented"
    | Expr.Ident _ -> failwith "Not Implemented"
    | Expr.OptVar _ -> failwith "Not Implemented"
    | Expr.LongIdentSet _ -> failwith "Not Implemented"
    | Expr.DotIndexedGet _ -> failwith "Not Implemented"
    | Expr.DotIndexedSet _ -> failwith "Not Implemented"
    | Expr.NamedIndexedPropertySet _ -> failwith "Not Implemented"
    | Expr.DotNamedIndexedPropertySet _ -> failwith "Not Implemented"
    | Expr.DotGet _ -> failwith "Not Implemented"
    | Expr.DotSet _ -> failwith "Not Implemented"
    | Expr.Set _ -> failwith "Not Implemented"
    | Expr.LibraryOnlyStaticOptimization _ -> failwith "Not Implemented"
    | Expr.InterpolatedStringExpr _ -> failwith "Not Implemented"
    | Expr.IndexRangeWildcard _ -> failwith "Not Implemented"
    | Expr.IndexRange _ -> failwith "Not Implemented"
    | Expr.IndexFromEnd _ -> failwith "Not Implemented"
    | Expr.Typar _ -> failwith "Not Implemented"
    |> genNode (Expr.Node e)

let genPat (p: Pattern) =
    match p with
    | Pattern.OptionalVal _ -> failwith "Not Implemented"
    | Pattern.Attrib _ -> failwith "Not Implemented"
    | Pattern.Or _ -> failwith "Not Implemented"
    | Pattern.Ands _ -> failwith "Not Implemented"
    | Pattern.Null _ -> failwith "Not Implemented"
    | Pattern.Wild _ -> failwith "Not Implemented"
    | Pattern.Typed _ -> failwith "Not Implemented"
    | Pattern.Named node -> genSingleTextNode node.Name
    | Pattern.As _ -> failwith "Not Implemented"
    | Pattern.ListCons _ -> failwith "Not Implemented"
    | Pattern.NamePatPairs _ -> failwith "Not Implemented"
    | Pattern.LongIdentParen _ -> failwith "Not Implemented"
    | Pattern.LongIdent _ -> failwith "Not Implemented"
    | Pattern.Unit _ -> failwith "Not Implemented"
    | Pattern.Paren _ -> failwith "Not Implemented"
    | Pattern.Tuple _ -> failwith "Not Implemented"
    | Pattern.StructTuple _ -> failwith "Not Implemented"
    | Pattern.ArrayOrList _ -> failwith "Not Implemented"
    | Pattern.Record _ -> failwith "Not Implemented"
    | Pattern.Const _ -> failwith "Not Implemented"
    | Pattern.IsInst _ -> failwith "Not Implemented"
    | Pattern.QuoteExpr _ -> failwith "Not Implemented"
    |> genNode (Pattern.Node p)

let genBinding (b: BindingNode) =
    genSingleTextNode b.LeadingKeyword
    +> sepSpace
    +> genSingleTextNode b.FunctionName
    +> sepSpace
    +> col sepSpace b.Parameters genPat
    +> sepSpace
    +> genSingleTextNode b.Equals
    +> sepSpace
    +> genExpr b.Expr
    |> genNode b

let genOpenList (openList: OpenListNode) =
    col sepNln openList.Opens (function
        | Open.ModuleOrNamespace node -> !- "open " +> genIdentListNode node.Name |> genNode node
        | Open.Target node -> !- "open type " +> genType node.Target)

let genType (t: Type) =
    match t with
    | Type.Funs _ -> failwith "Not Implemented"
    | Type.Tuple _ -> failwith "Not Implemented"
    | Type.HashConstraint _ -> failwith "Not Implemented"
    | Type.MeasurePower _ -> failwith "Not Implemented"
    | Type.MeasureDivide _ -> failwith "Not Implemented"
    | Type.StaticConstant _ -> failwith "Not Implemented"
    | Type.StaticConstantExpr _ -> failwith "Not Implemented"
    | Type.StaticConstantNamed _ -> failwith "Not Implemented"
    | Type.Array _ -> failwith "Not Implemented"
    | Type.Anon _ -> failwith "Not Implemented"
    | Type.Var _ -> failwith "Not Implemented"
    | Type.App _ -> failwith "Not Implemented"
    | Type.LongIdentApp _ -> failwith "Not Implemented"
    | Type.StructTuple _ -> failwith "Not Implemented"
    | Type.WithGlobalConstraints _ -> failwith "Not Implemented"
    | Type.LongIdent idn -> genIdentListNode idn
    | Type.AnonRecord _ -> failwith "Not Implemented"
    | Type.Paren _ -> failwith "Not Implemented"
    | Type.SignatureParameter _ -> failwith "Not Implemented"
    | Type.Or _ -> failwith "Not Implemented"

let genTypeDefn (td: TypeDefn) =
    let header =
        let node = (TypeDefn.TypeDefnNode td).TypeName

        genSingleTextNode node.LeadingKeyword
        +> sepSpace
        +> genIdentListNode node.Identifier
        +> sepSpace
        +> optSingle genSingleTextNode node.EqualsToken

    let body =
        match td with
        | TypeDefn.Enum _ -> failwith "Not Implemented"
        | TypeDefn.Union _ -> failwith "Not Implemented"
        | TypeDefn.Record _ -> failwith "Not Implemented"
        | TypeDefn.None _ -> failwith "Not Implemented"
        | TypeDefn.Abbrev node -> genType node.Type
        | TypeDefn.Exception _ -> failwith "Not Implemented"
        | TypeDefn.ExplicitClassOrInterfaceOrStruct _ -> failwith "Not Implemented"
        | TypeDefn.Augmentation _ -> failwith "Not Implemented"
        | TypeDefn.Fun _ -> failwith "Not Implemented"
        | TypeDefn.Delegate _ -> failwith "Not Implemented"
        | TypeDefn.Unspecified _ -> failwith "Not Implemented"
        | TypeDefn.RegularType _ -> failwith "Not Implemented"

    leadingExpressionIsMultiline header (fun isMultiline ->
        ifElse isMultiline (indentSepNlnUnindent body) (sepSpace +> body))
    |> genNode (TypeDefn.Node td)

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.OpenList ol -> genOpenList ol
    | ModuleDecl.HashDirectiveList _ -> failwith "Not Implemented"
    | ModuleDecl.AttributesList _ -> failwith "Not Implemented"
    | ModuleDecl.DeclExpr _ -> failwith "Not Implemented"
    | ModuleDecl.ExternBinding _ -> failwith "Not Implemented"
    | ModuleDecl.TopLevelBinding b -> genBinding b
    | ModuleDecl.ModuleAbbrev _ -> failwith "Not Implemented"
    | ModuleDecl.NestedModule _ -> failwith "Not Implemented"
    | ModuleDecl.TypeDefn td -> genTypeDefn td

let sepNlnUnlessContentBefore (node: NodeBase) =
    if Seq.isEmpty node.ContentBefore then sepNln else sepNone

let colWithNlnWhenMappedNodeIsMultiline<'n> (mapNode: 'n -> NodeBase) (f: 'n -> Context -> Context) (nodes: 'n list) =
    nodes
    |> List.map (fun n -> ColMultilineItem(f n, (mapNode >> sepNlnUnlessContentBefore) n))
    |> colWithNlnWhenItemIsMultiline

let colWithNlnWhenNodeIsMultiline<'n when 'n :> NodeBase> (f: 'n -> Context -> Context) (nodes: 'n list) =
    colWithNlnWhenMappedNodeIsMultiline<'n> (fun n -> n :> NodeBase) f nodes

let genModule (m: ModuleOrNamespaceNode) =
    onlyIf
        m.IsNamed
        (optSingle
            (fun (n: SingleTextNode) -> genSingleTextNode n +> sepSpace +> genIdentListNode m.Name)
            m.LeadingKeyword
         +> onlyIf (not m.Declarations.IsEmpty) (sepNln +> sepNln))
    +> colWithNlnWhenMappedNodeIsMultiline ModuleDecl.Node genModuleDecl m.Declarations
    |> genNode m

let genFile (oak: Oak) =
    col sepNln oak.ParsedHashDirectives genParsedHashDirective
    +> (if oak.ParsedHashDirectives.IsEmpty then sepNone else sepNln)
    +> col sepNln oak.ModulesOrNamespaces genModule
    +> (fun ctx -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx)
    |> genNode oak
