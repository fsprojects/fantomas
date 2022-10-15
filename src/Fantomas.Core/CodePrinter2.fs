module internal Fantomas.Core.CodePrinter2

open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

type IdentNode with

    member x.AddEvent = !-x.Text

type SingleTokenNode with

    member x.AddEvent = !-x.Token

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

let genIdentListNode (iln: IdentListNode) =
    col sepNone iln.Content (function
        | IdentifierOrDot.Ident ident -> ident.AddEvent
        | IdentifierOrDot.KnownDot _
        | IdentifierOrDot.UnknownDot _ -> sepDot)
    |> genNode iln

let genParsedHashDirective (phd: ParsedHashDirectiveNode) =
    !- "#" +> !-phd.Ident +> sepSpace +> col sepSpace phd.Args (fun a -> a.AddEvent)
    |> genNode phd

let genExpr (e: Expr) =
    match e with
    | Expr.Constant c -> c.AddEvent
    |> genNode e.Node

let genBinding (b: BindingNode) =
    b.LeadingKeyword.AddEvent
    +> sepSpace
    +> b.FunctionName.AddEvent
    +> sepSpace
    +> col sepSpace b.Parameters (fun _ -> sepNone)
    +> b.Equals.AddEvent
    +> sepSpace
    +> genExpr b.Expr
    |> genNode b

let genOpenList (openList: OpenListNode) =
    col sepNln openList.Opens (function
        | Open.ModuleOrNamespace node -> !- "open " +> genIdentListNode node.Name |> genNode node
        | Open.Target node -> !- "todo!")

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.TopLevelBinding b -> genBinding b
    | ModuleDecl.OpenList ol -> genOpenList ol

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
        (optSingle (fun (n: IdentNode) -> n.AddEvent +> sepSpace +> genIdentListNode m.Name) m.LeadingKeyword
         +> onlyIf (not m.Declarations.IsEmpty) (sepNln +> sepNln))
    +> colWithNlnWhenMappedNodeIsMultiline ModuleDecl.Node genModuleDecl m.Declarations
    |> genNode m

let genFile (oak: Oak) =
    col sepNln oak.ParsedHashDirectives genParsedHashDirective
    +> (if oak.ParsedHashDirectives.IsEmpty then sepNone else sepNln)
    +> col sepNln oak.ModulesOrNamespaces genModule
    +> (fun ctx -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx)
    |> genNode oak
