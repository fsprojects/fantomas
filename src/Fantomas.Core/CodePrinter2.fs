module internal Fantomas.Core.CodePrinter2

open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

type IdentNode with

    member x.AddEvent = !-x.Text

type SingleTokenNode with

    member x.AddEvent = !-x.Token

let genTrivia (trivia: TriviaNode) =
    match trivia.Content with
    | CommentOnSingleLine comment -> !-comment +> sepNln

let genNode<'n when 'n :> NodeBase> (n: 'n) (f: 'n -> Context -> Context) =
    col sepNln n.ContentBefore genTrivia
    +> f n
    +> col sepNln n.ContentAfter genTrivia

let genIdentListNode (iln: IdentListNode) =
    genNode iln (fun iln ->
        col sepNone iln.Content (function
            | IdentifierOrDot.Ident ident -> ident.AddEvent
            | IdentifierOrDot.KnownDot _
            | IdentifierOrDot.UnknownDot _ -> sepDot))

let genExpr (e: Expr) =
    genNode e.Node (fun _ ->
        match e with
        | Expr.Constant c -> c.AddEvent)

let genBinding (b: Binding) =
    genNode b (fun b ->
        b.LeadingKeyword.AddEvent
        +> sepSpace
        +> b.FunctionName.AddEvent
        +> sepSpace
        +> col sepSpace b.Parameters (fun _ -> sepNone)
        +> b.Equals.AddEvent
        +> sepSpace
        +> genExpr b.Expr)

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.TopLevelBinding b -> genBinding b

let genModule (m: ModuleOrNamespace) =
    genNode m (fun m ->
        onlyIf
            m.IsNamed
            (optSingle (fun (n: IdentNode) -> n.AddEvent +> sepSpace +> genIdentListNode m.Name) m.LeadingKeyword
             +> sepNln
             +> sepNln)
        +> col (sepNln +> sepNln) m.Declarations genModuleDecl)

let genFile (oak: Oak) =
    genNode oak (fun oak ->
        col sepNln oak.ModulesOrNamespaces genModule
        +> (fun ctx -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx))
