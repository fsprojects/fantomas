module rec Fantomas.Core.SyntaxOak

open System.Collections.Generic
open FSharp.Compiler.Text
open Microsoft.FSharp.Collections

// Open questions:
// - Do we need to distinguish between SignatureFile and ImplementationFile?

type TriviaContent =
    | CommentOnSingleLine of string
    | Newline

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

type StringNode(content: string, range: range) =
    inherit NodeBase(range)
    member x.Content = content
    override this.Children = Array.empty

let noa<'n when 'n :> NodeBase> (n: 'n option) =
    match n with
    | None -> Array.empty
    | Some n -> [| n :> NodeBase |]

let nodes<'n when 'n :> NodeBase> (ns: seq<'n>) = Seq.cast<NodeBase> ns

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

type IdentListNode(content: IdentifierOrDot list, range) =
    inherit NodeBase(range)
    member x.IsEmpty = content.IsEmpty
    member x.Content = content
    static member Empty = IdentListNode(List.empty, Range.Zero)

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

type Oak(parsedHashDirectives: ParsedHashDirectiveNode list, modulesOrNamespaces: ModuleOrNamespaceNode list) =
    inherit NodeBase([| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]
                     |> Seq.map nodeRange
                     |> combineRanges)

    member x.ParsedHashDirectives = parsedHashDirectives
    member x.ModulesOrNamespaces = modulesOrNamespaces

    override this.Children =
        [| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]

type ParsedHashDirectiveNode(ident: string, args: IdentNode list, range) =
    inherit NodeBase(range)
    member x.Ident = ident
    member x.Args = args
    override this.Children = [| yield! nodes args |]

type ModuleOrNamespaceNode(leadingKeyword: IdentNode option, name: IdentListNode, decls: ModuleDecl list, range) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.Name = name
    member x.Declarations = decls
    member x.IsNamed = Option.isSome x.LeadingKeyword

    override this.Children =
        [| yield! noa leadingKeyword
           yield name
           yield! List.map ModuleDecl.Node decls |]

type Type =
    | Todo

    static member Node(x: Type) : NodeBase =
        match x with
        | Type.Todo _ -> failwith "really, todo"

type OpenModuleOrNamespaceNode(identListNode: IdentListNode, range) =
    inherit NodeBase(range)

    override this.Children = Array.empty
    member x.Name = identListNode

type OpenTargetNode(target: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Type.Node target |]

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Open =
    | ModuleOrNamespace of OpenModuleOrNamespaceNode
    | Target of OpenTargetNode

    static member Node(x: Open) : NodeBase =
        match x with
        | ModuleOrNamespace n -> n
        | Target n -> n

type OpenListNode(opens: Open list) =
    inherit NodeBase(List.map (Open.Node >> nodeRange) opens |> combineRanges)

    override this.Children = [| yield! (List.map Open.Node opens) |]
    member x.Opens = opens

/// Each case in this DU should have a container node
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ModuleDecl =
    | TopLevelBinding of BindingNode
    | OpenList of OpenListNode

    static member Node(x: ModuleDecl) =
        match x with
        | TopLevelBinding n -> n
        | OpenList n -> n

type BindingNode
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

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
    | Constant of IdentNode

    member x.Node: NodeBase =
        match x with
        | Constant n -> n
