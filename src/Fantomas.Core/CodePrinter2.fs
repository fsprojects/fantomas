module internal Fantomas.Core.CodePrinter2

open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

type IdentNode with

    member x.AddEvent = !-x.Text

type SingleTokenNode with

    member x.AddEvent = !-x.Token

let genIdentListNode (iln: IdentListNode) =
    col sepNone iln.Content (function
        | IdentifierOrDot.Ident ident -> ident.AddEvent
        | IdentifierOrDot.KnownDot _
        | IdentifierOrDot.UnknownDot _ -> sepDot)

let genExpr (e: Expr) =
    match e with
    | Expr.Constant c -> c.AddEvent

let genBinding (b: Binding) =
    b.LeadingKeyword.AddEvent
    +> sepSpace
    +> b.FunctionName.AddEvent
    +> sepSpace
    +> col sepSpace b.Parameters (fun _ -> sepNone)
    +> b.Equals.AddEvent
    +> sepSpace
    +> genExpr b.Expr

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.TopLevelBinding b -> genBinding b

let genModule (m: ModuleOrNamespace) =
    onlyIf
        m.IsNamed
        (optSingle (fun (n: IdentNode) -> n.AddEvent +> sepSpace +> genIdentListNode m.Name) m.LeadingKeyword
         +> sepNln
         +> sepNln)
    +> col sepNln m.Declarations genModuleDecl

let genFile (oak: FileNode) =
    col sepNln oak.ModulesOrNamespaces genModule
    +> (fun ctx -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx)
