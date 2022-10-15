module rec Fantomas.Core.SyntaxOak

open FSharp.Compiler.Text

// Open questions:
// - Do we need to distinguish between SignatureFile and ImplementationFile?

type Trivia = obj

type NodeBase(range: range) =
    member _.ContentBefore() : Trivia list = []
    member _.ContentAfter() : Trivia list = []
    member _.Range = range

let nodeRange (n: NodeBase) = n.Range

let combineRanges (ranges: range seq) =
    if Seq.isEmpty ranges then
        Range.Zero
    else
        Seq.reduce Range.unionRanges ranges

type DotNode(range) =
    inherit NodeBase(range)

[<RequireQualifiedAccess>]
type IdentifierOrDot =
    | Ident of IdentNode
    | KnownDot of DotNode
    | UnknownDot

    member x.Range =
        match x with
        | Ident n -> Some n.Range
        | KnownDot n -> Some n.Range
        | UnknownDot -> None

type IdentListNode(content: IdentifierOrDot list) =
    inherit NodeBase(content |> Seq.choose (fun iod -> iod.Range) |> combineRanges)
    member x.IsEmpty = content.IsEmpty
    member x.Content = content
    static member Empty = IdentListNode(List.empty)

type IdentNode(idText: string, range: range) =
    inherit NodeBase(range)
    member _.Text = idText

type SingleTokenNode(token: string, range) =
    inherit NodeBase(range)
    member _.Token = token

type FileNode(modulesOrNamespaces: ModuleOrNamespace list) =
    inherit NodeBase(modulesOrNamespaces |> Seq.map nodeRange |> combineRanges)
    member x.ModulesOrNamespaces = modulesOrNamespaces

type ModuleOrNamespace(leadingKeyword: IdentNode option, name: IdentListNode, decls: ModuleDecl list, range) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.Name = name
    member x.Declarations = decls
    member x.IsNamed = Option.isSome x.LeadingKeyword

type ModuleDecl = TopLevelBinding of Binding

type Binding
    (
        leadingKeyword: IdentNode,
        functionName: IdentNode,
        parameters: obj seq,
        equals: SingleTokenNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.FunctionName = functionName
    member x.Parameters = parameters
    member x.Equals = equals
    member x.Expr = expr

[<RequireQualifiedAccess>]
type Expr = Constant of IdentNode
