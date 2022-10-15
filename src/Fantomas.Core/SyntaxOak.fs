module rec Fantomas.Core.SyntaxOak

open System.Collections.Generic
open FSharp.Compiler.Text
open Microsoft.FSharp.Collections

// Open questions:
// - Do we need to distinguish between SignatureFile and ImplementationFile?

type TriviaContent = CommentOnSingleLine of string

type TriviaNode(content: TriviaContent, range: range) =
    member x.Content = content
    member x.Range = range

[<AbstractClass>]
type NodeBase(range: range) =
    let nodesBefore = Queue<TriviaNode>(0)
    let nodesAfter = Queue<TriviaNode>(0)

    member _.ContentBefore: TriviaNode seq = nodesBefore
    member _.ContentAfter: TriviaNode seq = nodesAfter
    member _.Range = range
    abstract member Children: NodeBase array
    member _.AddBefore(triviaNode: TriviaNode) = nodesBefore.Enqueue triviaNode
    member _.AddAfter(triviaNode: TriviaNode) = nodesAfter.Enqueue triviaNode

let noa<'n when 'n :> NodeBase> (n: 'n option) =
    match n with
    | None -> Array.empty
    | Some n -> [| n :> NodeBase |]

let nodeRange (n: NodeBase) = n.Range

let combineRanges (ranges: range seq) =
    if Seq.isEmpty ranges then
        Range.Zero
    else
        Seq.reduce Range.unionRanges ranges

type DotNode(range) =
    inherit NodeBase(range)

    override x.Children = Array.empty

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

    override x.Children =
        x.Content
        |> List.choose (function
            | IdentifierOrDot.Ident n -> Some(n :> NodeBase)
            | _ -> None)
        |> Array.ofList

type IdentNode(idText: string, range: range) =
    inherit NodeBase(range)
    member _.Text = idText
    override x.Children = Array.empty

type SingleTokenNode(token: string, range) =
    inherit NodeBase(range)
    member _.Token = token
    override x.Children = Array.empty

type Oak(modulesOrNamespaces: ModuleOrNamespace list) =
    inherit NodeBase(modulesOrNamespaces |> Seq.map nodeRange |> combineRanges)
    member x.ModulesOrNamespaces = modulesOrNamespaces
    override this.Children = modulesOrNamespaces |> Seq.cast<NodeBase> |> Seq.toArray

type ModuleOrNamespace(leadingKeyword: IdentNode option, name: IdentListNode, decls: ModuleDecl list, range) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.Name = name
    member x.Declarations = decls
    member x.IsNamed = Option.isSome x.LeadingKeyword

    override this.Children =
        [| yield! noa leadingKeyword
           yield name
           yield! List.map ModuleDecl.Node decls |]

type ModuleDecl =
    | TopLevelBinding of Binding

    static member Node(x: ModuleDecl) =
        match x with
        | TopLevelBinding n -> n

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

    override this.Children =
        [| yield leadingKeyword; yield functionName; yield equals; yield expr.Node |]

[<RequireQualifiedAccess>]
type Expr =
    | Constant of IdentNode

    member x.Node: NodeBase =
        match x with
        | Constant n -> n
