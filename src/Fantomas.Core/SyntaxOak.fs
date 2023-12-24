module rec Fantomas.Core.SyntaxOak

open System.Collections.Generic
open Fantomas.FCS.Text

type TriviaContent =
    | CommentOnSingleLine of string
    | LineCommentAfterSourceCode of comment: string
    | BlockComment of comment: string * newlineBefore: bool * newlineAfter: bool
    | Newline
    | Directive of string
    | Cursor

type TriviaNode(content: TriviaContent, range: range) =
    member val Content: TriviaContent = content
    member val Range: range = range

[<Interface>]
type Node =
    abstract ContentBefore: TriviaNode seq
    abstract HasContentBefore: bool
    abstract ContentAfter: TriviaNode seq
    abstract HasContentAfter: bool
    abstract Range: range
    abstract Children: Node array
    abstract AddBefore: triviaNode: TriviaNode -> unit
    abstract AddAfter: triviaNode: TriviaNode -> unit
    abstract AddCursor: pos -> unit
    abstract TryGetCursor: pos option

[<AbstractClass>]
type NodeBase(range: range) =
    let mutable potentialCursor = None
    let nodesBefore = Queue<TriviaNode>(0)
    let nodesAfter = Queue<TriviaNode>(0)

    member _.ContentBefore: TriviaNode seq = nodesBefore

    member _.HasContentBefore: bool =
        nodesBefore
        |> Seq.filter (fun tn ->
            match tn.Content with
            | Cursor -> false
            | _ -> true)
        |> Seq.isEmpty
        |> not

    member _.ContentAfter: TriviaNode seq = nodesAfter

    member _.HasContentAfter: bool =
        nodesAfter
        |> Seq.filter (fun tn ->
            match tn.Content with
            | Cursor -> false
            | _ -> true)
        |> Seq.isEmpty
        |> not

    member _.Range: range = range
    member _.AddBefore(triviaNode: TriviaNode) : unit = nodesBefore.Enqueue triviaNode
    member _.AddAfter(triviaNode: TriviaNode) : unit = nodesAfter.Enqueue triviaNode
    abstract member Children: Node array
    member _.AddCursor(cursor: pos) : unit = potentialCursor <- Some cursor
    member _.TryGetCursor: pos option = potentialCursor

    interface Node with
        member x.ContentBefore = x.ContentBefore
        member x.HasContentBefore = x.HasContentBefore
        member x.ContentAfter = x.ContentAfter
        member x.HasContentAfter = x.HasContentAfter
        member x.Range = x.Range
        member x.AddBefore triviaNode = x.AddBefore triviaNode
        member x.AddAfter triviaNode = x.AddAfter triviaNode
        member x.Children = x.Children
        member x.AddCursor cursor = x.AddCursor cursor
        member x.TryGetCursor = x.TryGetCursor

type StringNode(content: string, range: range) =
    inherit NodeBase(range)
    member val Content: string = content
    override val Children: SyntaxOak.Node array = Array.empty

let noa<'n when 'n :> Node> (n: 'n option) : Node array =
    match n with
    | None -> Array.empty
    | Some n -> [| n :> Node |]

let nodes<'n when 'n :> Node> (ns: 'n seq) : Node seq = Seq.cast<Node> ns

let nodeRange (n: Node) : range = n.Range

let combineRanges (ranges: range seq) : range =
    if Seq.isEmpty ranges then
        Range.Zero
    else
        Seq.reduce Range.unionRanges ranges

[<RequireQualifiedAccess; NoComparison>]
type IdentifierOrDot =
    | Ident of SingleTextNode
    | KnownDot of SingleTextNode
    | UnknownDot

    member x.Range: range option =
        match x with
        | Ident n -> Some n.Range
        | KnownDot n -> Some n.Range
        | UnknownDot -> None

type IdentListNode(content: IdentifierOrDot list, range: range) =
    inherit NodeBase(range)
    member val IsEmpty: bool = content.IsEmpty
    member val Content: IdentifierOrDot list = content
    static member Empty: IdentListNode = IdentListNode(List.empty, Range.Zero)

    override x.Children: SyntaxOak.Node array =
        x.Content
        |> List.choose (function
            | IdentifierOrDot.Ident n -> Some(n :> Node)
            | IdentifierOrDot.KnownDot n -> Some(n :> Node)
            | _ -> None)
        |> Array.ofList

type SingleTextNode(idText: string, range: range) =
    inherit NodeBase(range)
    member val Text: string = idText
    override val Children: SyntaxOak.Node array = Array.empty

type MultipleTextsNode(content: SingleTextNode list, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! nodes content |]
    member val Content: SingleTextNode list = content

type XmlDocNode(lines: string array, range: range) =

    inherit NodeBase(range)
    override val Children: SyntaxOak.Node array = Array.empty
    member val Lines: string array = lines

type Oak(parsedHashDirectives: ParsedHashDirectiveNode list, modulesOrNamespaces: ModuleOrNamespaceNode list, m: range)
    =
    inherit NodeBase(m)

    member val ParsedHashDirectives: ParsedHashDirectiveNode list = parsedHashDirectives
    member val ModulesOrNamespaces: ModuleOrNamespaceNode list = modulesOrNamespaces

    override val Children: Node array = [| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]

type ParsedHashDirectiveNode(ident: string, args: SingleTextNode list, range: range) =
    inherit NodeBase(range)
    member val Ident: string = ident
    member val Args: SingleTextNode list = args
    override val Children: Node array = [| yield! nodes args |]

type ModuleOrNamespaceHeaderNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        accessibility: SingleTextNode option,
        isRecursive: bool,
        name: IdentListNode option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa accessibility
           yield! noa name |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode = leadingKeyword
    member val Accessibility: SingleTextNode option = accessibility
    member val IsRecursive: bool = isRecursive
    member val Name: IdentListNode option = name

type ModuleOrNamespaceNode(header: ModuleOrNamespaceHeaderNode option, decls: ModuleDecl list, range: range) =
    inherit NodeBase(range)
    member val Declarations: ModuleDecl list = decls
    member val IsNamed: bool = Option.isSome header

    override val Children: Node array = [| yield! noa header; yield! List.map ModuleDecl.Node decls |]
    member val Header: ModuleOrNamespaceHeaderNode option = header

type TypeFunsNode(parameters: (Type * SingleTextNode) list, returnType: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! nodes (List.collect (fun (t, arrow) -> [ yield Type.Node t; yield (arrow :> Node) ]) parameters)
           yield Type.Node returnType |]

    /// Type + arrow
    member val Parameters: (Type * SingleTextNode) list = parameters
    member val ReturnType: Type = returnType

type TypeTupleNode(path: Choice<Type, SingleTextNode> list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 n -> n :> Node)
                   path |]

    member val Path: Choice<Type, SingleTextNode> list = path

type TypeHashConstraintNode(hash: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield hash; yield Type.Node t |]
    member val Hash: SingleTextNode = hash
    member val Type: Type = t

type TypeMeasurePowerNode(baseMeasure: Type, exponent: RationalConstNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node baseMeasure |]
    member val BaseMeasure: Type = baseMeasure
    member val Exponent: RationalConstNode = exponent

type TypeStaticConstantExprNode(constNode: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield constNode; yield Expr.Node expr |]
    member val Const: SingleTextNode = constNode
    member val Expr: Expr = expr

type TypeStaticConstantNamedNode(identifier: Type, value: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node identifier; yield Type.Node value |]
    member val Identifier: Type = identifier
    member val Value: Type = value

type TypeArrayNode(t: Type, rank: int, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node t |]
    member val Type: Type = t
    member val Rank: int = rank

type TypeAppPostFixNode(first: Type, last: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node first; yield Type.Node last |]
    member val First: Type = first
    member val Last: Type = last

type TypeAppPrefixNode
    (
        identifier: Type,
        postIdentifier: IdentListNode option,
        lessThan: SingleTextNode,
        arguments: Type list,
        greaterThan: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Type.Node identifier
           yield! noa postIdentifier
           yield lessThan
           yield! (List.map Type.Node arguments)
           yield greaterThan |]

    member val Identifier: Type = identifier
    member val PostIdentifier: IdentListNode option = postIdentifier
    member val GreaterThan: SingleTextNode = greaterThan
    member val Arguments: Type list = arguments
    member val LessThen: SingleTextNode = lessThan

type TypeStructTupleNode
    (keyword: SingleTextNode, path: Choice<Type, SingleTextNode> list, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield keyword
           yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 n -> n :> Node)
                   path
           yield closingParen |]

    member val Keyword: SingleTextNode = keyword
    member val Path: Choice<Type, SingleTextNode> list = path
    member val ClosingParen: SingleTextNode = closingParen

type TypeWithGlobalConstraintsNode(t: Type, constraints: TypeConstraint list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node t; yield! List.map TypeConstraint.Node constraints |]

    member val Type: Type = t
    member val TypeConstraints: TypeConstraint list = constraints

type TypeAnonRecordNode
    (
        structNode: SingleTextNode option,
        openingToken: SingleTextNode option,
        fields: (SingleTextNode * Type) list,
        closingToken: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa structNode
           yield! noa openingToken
           yield! (fields |> List.collect (fun (i, t) -> [ yield (i :> Node); yield Type.Node t ]))
           yield closingToken |]

    member val Struct: SingleTextNode option = structNode
    member val Opening: SingleTextNode option = openingToken
    member val Fields: (SingleTextNode * Type) list = fields
    member val Closing: SingleTextNode = closingToken

type TypeParenNode(openingParen: SingleTextNode, t: Type, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield Type.Node t; yield closingParen |]
    member val OpeningParen: SingleTextNode = openingParen
    member val Type: Type = t
    member val ClosingParen: SingleTextNode = closingParen

type TypeSignatureParameterNode
    (attributes: MultipleAttributeListNode option, identifier: SingleTextNode option, t: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! noa attributes; yield! noa identifier; yield Type.Node t |]

    member val Attributes: MultipleAttributeListNode option = attributes
    member val Identifier: SingleTextNode option = identifier
    member val Type: Type = t

type TypeOrNode(lhs: Type, orNode: SingleTextNode, rhs: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node lhs; yield orNode; yield Type.Node rhs |]
    member val LeftHandSide: Type = lhs
    member val Or: SingleTextNode = orNode
    member val RightHandSide: Type = rhs

type TypeLongIdentAppNode(appType: Type, longIdent: IdentListNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node appType; yield longIdent |]
    member val AppType: Type = appType
    member val LongIdent: IdentListNode = longIdent

type TypeIntersectionNode(typesAndSeparators: Choice<Type, SingleTextNode> list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| for t in typesAndSeparators do
               match t with
               | Choice1Of2 t -> Type.Node t
               | Choice2Of2 amp -> amp |]

    member val TypesAndSeparators: Choice<Type, SingleTextNode> list = typesAndSeparators

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Type =
    | Funs of TypeFunsNode
    | Tuple of TypeTupleNode
    | HashConstraint of TypeHashConstraintNode
    | MeasurePower of TypeMeasurePowerNode
    | StaticConstant of Constant
    | StaticConstantExpr of TypeStaticConstantExprNode
    | StaticConstantNamed of TypeStaticConstantNamedNode
    | Array of TypeArrayNode
    | Anon of SingleTextNode
    | Var of SingleTextNode
    | AppPostfix of TypeAppPostFixNode
    | AppPrefix of TypeAppPrefixNode
    | StructTuple of TypeStructTupleNode
    | WithSubTypeConstraint of TypeConstraint
    | WithGlobalConstraints of TypeWithGlobalConstraintsNode
    | LongIdent of IdentListNode
    | AnonRecord of TypeAnonRecordNode
    | Paren of TypeParenNode
    | SignatureParameter of TypeSignatureParameterNode
    | Or of TypeOrNode
    | LongIdentApp of TypeLongIdentAppNode
    | Intersection of TypeIntersectionNode

    static member Node(x: Type) : Node =
        match x with
        | Funs n -> n
        | Tuple n -> n
        | HashConstraint n -> n
        | MeasurePower n -> n
        | StaticConstant c -> Constant.Node c
        | StaticConstantExpr n -> n
        | StaticConstantNamed n -> n
        | Array n -> n
        | Anon n -> n
        | Var n -> n
        | AppPostfix n -> n
        | AppPrefix n -> n
        | StructTuple n -> n
        | WithSubTypeConstraint tc -> TypeConstraint.Node tc
        | WithGlobalConstraints n -> n
        | LongIdent n -> n
        | AnonRecord n -> n
        | Paren n -> n
        | SignatureParameter n -> n
        | Or n -> n
        | LongIdentApp n -> n
        | Intersection n -> n

/// A pattern composed from a left hand-side pattern, a single text token/operator and a right hand-side pattern.
type PatLeftMiddleRight(lhs: Pattern, middle: Choice<SingleTextNode, string>, rhs: Pattern, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Pattern.Node lhs
           match middle with
           | Choice1Of2 n -> yield n
           | _ -> ()
           yield Pattern.Node rhs |]

    member val LeftHandSide: Pattern = lhs
    member val Middle: Choice<SingleTextNode, string> = middle
    member val RightHandSide: Pattern = rhs

type PatAndsNode(pats: Pattern list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! List.map Pattern.Node pats |]
    member val Patterns: Pattern list = pats

type PatParameterNode(attributes: MultipleAttributeListNode option, pat: Pattern, t: Type option, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield Pattern.Node pat
           yield! noa (Option.map Type.Node t) |]

    member val Attributes: MultipleAttributeListNode option = attributes
    member val Pattern: Pattern = pat
    member val Type: Type option = t

type PatNamedParenStarIdentNode
    (
        accessibility: SingleTextNode option,
        openingParen: SingleTextNode,
        name: SingleTextNode,
        closingParen: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa accessibility
           yield openingParen
           yield name
           yield closingParen |]

    member val Accessibility: SingleTextNode option = accessibility
    member val OpeningParen: SingleTextNode = openingParen
    member val Name: SingleTextNode = name
    member val ClosingParen: SingleTextNode = closingParen

type PatNamedNode(accessibility: SingleTextNode option, name: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield name |]
    member val Name: SingleTextNode = name
    member val Accessibility: SingleTextNode option = accessibility

type NamePatPair(ident: SingleTextNode, equals: SingleTextNode, pat: Pattern, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield ident; yield equals; yield Pattern.Node pat |]
    member val Ident: SingleTextNode = ident
    member val Equals: SingleTextNode = equals
    member val Pattern: Pattern = pat

type PatNamePatPairsNode
    (
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        openingParen: SingleTextNode,
        pairs: NamePatPair list,
        closingParen: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield openingParen
           yield! nodes pairs
           yield closingParen |]

    member val Identifier: IdentListNode = identifier
    member val TyparDecls: TyparDecls option = typarDecls
    member val OpeningParen: SingleTextNode = openingParen
    member val Pairs: NamePatPair list = pairs
    member val ClosingParen: SingleTextNode = closingParen

type PatLongIdentNode
    (
        accessibility: SingleTextNode option,
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        parameters: Pattern list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa accessibility
           yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield! List.map Pattern.Node parameters |]

    member val Accessibility: SingleTextNode option = accessibility
    member val Identifier: IdentListNode = identifier
    member val TyparDecls: TyparDecls option = typarDecls
    member val Parameters: Pattern list = parameters

type PatParenNode(openingParen: SingleTextNode, pat: Pattern, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield Pattern.Node pat; yield closingParen |]

    member val OpeningParen: SingleTextNode = openingParen
    member val Pattern: Pattern = pat
    member val ClosingParen: SingleTextNode = closingParen

type PatTupleNode(items: Choice<Pattern, SingleTextNode> list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| for item in items do
               match item with
               | Choice1Of2 p -> Pattern.Node p
               | Choice2Of2 comma -> comma |]

    member val Items: Choice<Pattern, SingleTextNode> list = items

type PatStructTupleNode(pats: Pattern list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! (List.map Pattern.Node pats) |]
    member val Patterns: Pattern list = pats

type PatArrayOrListNode(openToken: SingleTextNode, pats: Pattern list, closeToken: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openToken; yield! List.map Pattern.Node pats; yield closeToken |]

    member val OpenToken: SingleTextNode = openToken
    member val Patterns: Pattern list = pats
    member val CloseToken: SingleTextNode = closeToken

type PatRecordField
    (prefix: IdentListNode option, fieldName: SingleTextNode, equals: SingleTextNode, pat: Pattern, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! noa prefix; yield fieldName; yield equals; yield Pattern.Node pat |]

    member val Prefix: IdentListNode option = prefix
    member val FieldName: SingleTextNode = fieldName
    member val Equals: SingleTextNode = equals
    member val Pattern: Pattern = pat

type PatRecordNode(openingNode: SingleTextNode, fields: PatRecordField list, closingNode: SingleTextNode, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingNode; yield! nodes fields; yield closingNode |]
    member val OpeningNode: SingleTextNode = openingNode
    member val Fields: PatRecordField list = fields
    member val ClosingNode: SingleTextNode = closingNode

type PatIsInstNode(token: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield token; yield Type.Node t |]
    member val Token: SingleTextNode = token
    member val Type: Type = t

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Pattern =
    | OptionalVal of SingleTextNode
    | Or of PatLeftMiddleRight
    | Ands of PatAndsNode
    | Null of SingleTextNode
    | Wild of SingleTextNode
    | Parameter of PatParameterNode
    | NamedParenStarIdent of PatNamedParenStarIdentNode
    | Named of PatNamedNode
    | As of PatLeftMiddleRight
    | ListCons of PatLeftMiddleRight
    | NamePatPairs of PatNamePatPairsNode
    | LongIdent of PatLongIdentNode
    | Unit of UnitNode
    | Paren of PatParenNode
    | Tuple of PatTupleNode
    | StructTuple of PatStructTupleNode
    | ArrayOrList of PatArrayOrListNode
    | Record of PatRecordNode
    | Const of Constant
    | IsInst of PatIsInstNode
    | QuoteExpr of ExprQuoteNode

    static member Node(x: Pattern) : Node =
        match x with
        | OptionalVal n -> n
        | Parameter n -> n
        | Or n -> n
        | Ands n -> n
        | Null n -> n
        | Wild n -> n
        | NamedParenStarIdent n -> n
        | Named n -> n
        | As n -> n
        | ListCons n -> n
        | NamePatPairs n -> n
        | LongIdent n -> n
        | Unit n -> n
        | Paren n -> n
        | Tuple n -> n
        | StructTuple n -> n
        | ArrayOrList n -> n
        | Record n -> n
        | Const c -> Constant.Node c
        | IsInst n -> n
        | QuoteExpr n -> n

type ExprLazyNode(lazyWord: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield lazyWord; yield Expr.Node expr |]

    member val LazyWord: SingleTextNode = lazyWord
    member val Expr: Expr = expr

    member val ExprIsInfix: bool =
        match Expr.Node expr with
        | :? InfixApp -> true
        | _ -> false

type ExprSingleNode(leading: SingleTextNode, addSpace: bool, supportsStroustrup: bool, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield leading; yield Expr.Node expr |]

    member val Leading: SingleTextNode = leading
    member val AddSpace: bool = addSpace
    member val SupportsStroustrup: bool = supportsStroustrup
    member val Expr: Expr = expr

type ExprConstantNode(range: range) =
    inherit NodeBase(range)

    override val Children: Node array = failwith "todo"

type ExprQuoteNode(openToken: SingleTextNode, expr: Expr, closeToken: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openToken; yield Expr.Node expr; yield closeToken |]
    member val OpenToken: SingleTextNode = openToken
    member val Expr: Expr = expr
    member val CloseToken: SingleTextNode = closeToken

type ExprTypedNode(expr: Expr, operator: string, t: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node expr; yield Type.Node t |]
    member val Expr: Expr = expr
    member val Operator: string = operator
    member val Type: Type = t

type ExprNewNode(newKeyword: SingleTextNode, t: Type, arguments: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield newKeyword; yield Type.Node t; yield Expr.Node arguments |]

    member val NewKeyword: SingleTextNode = newKeyword
    member val Type: Type = t
    member val Arguments: Expr = arguments

type ExprTupleNode(items: Choice<Expr, SingleTextNode> list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        items
        |> Seq.map (function
            | Choice1Of2 e -> Expr.Node e
            | Choice2Of2 comma -> comma :> Node)
        |> Seq.toArray

    member val Items: Choice<Expr, SingleTextNode> list = items

type ExprStructTupleNode(structNode: SingleTextNode, tuple: ExprTupleNode, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield structNode; yield tuple; yield closingParen |]
    member val Struct: SingleTextNode = structNode
    member val Tuple: ExprTupleNode = tuple
    member val ClosingParen: SingleTextNode = closingParen

type ExprArrayOrListNode(openingToken: SingleTextNode, elements: Expr list, closingToken: SingleTextNode, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingToken; yield! List.map Expr.Node elements; yield closingToken |]

    member val Opening: SingleTextNode = openingToken
    member val Elements: Expr list = elements
    member val Closing: SingleTextNode = closingToken

type InheritConstructorTypeOnlyNode(inheritKeyword: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node t |]
    member val InheritKeyword: SingleTextNode = inheritKeyword
    member val Type: Type = t

type InheritConstructorUnitNode
    (inheritKeyword: SingleTextNode, t: Type, openingParen: SingleTextNode, closingParen: SingleTextNode, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield inheritKeyword
           yield Type.Node t
           yield openingParen
           yield closingParen |]

    member val InheritKeyword: SingleTextNode = inheritKeyword
    member val Type: Type = t
    member val OpeningParen: SingleTextNode = openingParen
    member val ClosingParen: SingleTextNode = closingParen

type InheritConstructorParenNode(inheritKeyword: SingleTextNode, t: Type, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node t; yield Expr.Node expr |]

    member val InheritKeyword: SingleTextNode = inheritKeyword
    member val Type: Type = t
    member val Expr: Expr = expr

type InheritConstructorOtherNode(inheritKeyword: SingleTextNode, t: Type, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node t; yield Expr.Node expr |]

    member val InheritKeyword: SingleTextNode = inheritKeyword
    member val Type: Type = t
    member val Expr: Expr = expr

[<RequireQualifiedAccess; NoComparison>]
type InheritConstructor =
    | TypeOnly of InheritConstructorTypeOnlyNode
    | Unit of InheritConstructorUnitNode
    | Paren of InheritConstructorParenNode
    | Other of InheritConstructorOtherNode

    static member Node(ic: InheritConstructor) : Node =
        match ic with
        | TypeOnly n -> n
        | Unit n -> n
        | Paren n -> n
        | Other n -> n

    member x.InheritKeyword: SingleTextNode =
        match x with
        | TypeOnly n -> n.InheritKeyword
        | Unit n -> n.InheritKeyword
        | Paren n -> n.InheritKeyword
        | Other n -> n.InheritKeyword

type RecordFieldNode(fieldName: IdentListNode, equals: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield fieldName; yield equals; yield Expr.Node expr |]
    member val FieldName: IdentListNode = fieldName
    member val Equals: SingleTextNode = equals
    member val Expr: Expr = expr

[<AbstractClass>]
type ExprRecordBaseNode
    (openingBrace: SingleTextNode, fields: RecordFieldNode list, closingBrace: SingleTextNode, range: range) =
    inherit NodeBase(range)

    member val OpeningBrace: SingleTextNode = openingBrace
    member val Fields: RecordFieldNode list = fields
    member val ClosingBrace: SingleTextNode = closingBrace
    member x.HasFields: bool = List.isNotEmpty x.Fields

/// <summary>
/// Represents a record instance, parsed from both `SynExpr.Record` and `SynExpr.AnonRecd`.
/// </summary>
type ExprRecordNode
    (
        openingBrace: SingleTextNode,
        copyInfo: Expr option,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range: range
    ) =
    inherit ExprRecordBaseNode(openingBrace, fields, closingBrace, range)

    member val CopyInfo: Expr option = copyInfo

    override val Children: Node array =
        [| yield openingBrace
           yield! copyInfo |> Option.map Expr.Node |> noa
           yield! nodes fields
           yield closingBrace |]

    member x.HasFields: bool = List.isNotEmpty x.Fields

type ExprAnonStructRecordNode
    (
        structNode: SingleTextNode,
        openingBrace: SingleTextNode,
        copyInfo: Expr option,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range: range
    ) =
    inherit ExprRecordNode(openingBrace, copyInfo, fields, closingBrace, range)
    member val Struct: SingleTextNode = structNode

    override val Children: Node array =
        [| yield structNode
           yield openingBrace
           yield! copyInfo |> Option.map Expr.Node |> noa
           yield! nodes fields
           yield closingBrace |]

type ExprInheritRecordNode
    (
        openingBrace: SingleTextNode,
        inheritConstructor: InheritConstructor,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range: range
    ) =
    inherit ExprRecordBaseNode(openingBrace, fields, closingBrace, range)

    member val InheritConstructor: InheritConstructor = inheritConstructor

    override val Children: Node array =
        [| yield openingBrace
           yield InheritConstructor.Node inheritConstructor
           yield! nodes fields
           yield closingBrace |]

type InterfaceImplNode
    (
        interfaceNode: SingleTextNode,
        t: Type,
        withNode: SingleTextNode option,
        bindings: BindingNode list,
        members: MemberDefn list,
        range: range
    ) =

    inherit NodeBase(range)

    override val Children: Node array =
        [| yield interfaceNode
           yield Type.Node t
           yield! noa withNode
           yield! nodes bindings
           yield! List.map MemberDefn.Node members |]

    member val Interface: SingleTextNode = interfaceNode
    member val Type: Type = t
    member val With: SingleTextNode option = withNode
    member val Bindings: BindingNode list = bindings
    member val Members: MemberDefn list = members

type ExprObjExprNode
    (
        openingBrace: SingleTextNode,
        newNode: SingleTextNode,
        t: Type,
        e: Expr option,
        withNode: SingleTextNode option,
        bindings: BindingNode list,
        members: MemberDefn list,
        interfaces: InterfaceImplNode list,
        closingBrace: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield openingBrace
           yield newNode
           yield Type.Node t
           yield! noa (Option.map Expr.Node e)
           yield! noa withNode
           yield! nodes bindings
           yield! List.map MemberDefn.Node members
           yield! nodes interfaces
           yield closingBrace |]

    member val OpeningBrace: SingleTextNode = openingBrace
    member val New: SingleTextNode = newNode
    member val Type: Type = t
    member val Expr: Expr option = e
    member val With: SingleTextNode option = withNode
    member val Bindings: BindingNode list = bindings
    member val Members: MemberDefn list = members
    member val Interfaces: InterfaceImplNode list = interfaces
    member val ClosingBrace: SingleTextNode = closingBrace

type ExprWhileNode(whileNode: SingleTextNode, whileExpr: Expr, doExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield whileNode; yield Expr.Node whileExpr; yield Expr.Node doExpr |]

    member val While: SingleTextNode = whileNode
    member val WhileExpr: Expr = whileExpr
    member val DoExpr: Expr = doExpr

type ExprForNode
    (
        forNode: SingleTextNode,
        ident: SingleTextNode,
        equals: SingleTextNode,
        identBody: Expr,
        direction: bool,
        toBody: Expr,
        doBody: Expr,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield forNode
           yield ident
           yield equals
           yield Expr.Node identBody
           yield Expr.Node toBody
           yield Expr.Node doBody |]

    member val For: SingleTextNode = forNode
    member val Ident: SingleTextNode = ident
    member val Equals: SingleTextNode = equals
    member val IdentBody: Expr = identBody
    member val Direction: bool = direction
    member val ToBody: Expr = toBody
    member val DoBody: Expr = doBody

type ExprForEachNode(forNode: SingleTextNode, pat: Pattern, enumExpr: Expr, isArrow: bool, bodyExpr: Expr, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield forNode
           yield Pattern.Node pat
           yield Expr.Node enumExpr
           yield Expr.Node bodyExpr |]

    member val For: SingleTextNode = forNode
    member val Pattern: Pattern = pat
    member val EnumExpr: Expr = enumExpr
    member val IsArrow: bool = isArrow
    member val BodyExpr: Expr = bodyExpr

type ExprNamedComputationNode
    (nameExpr: Expr, openingBrace: SingleTextNode, bodyExpr: Expr, closingBrace: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node nameExpr
           yield openingBrace
           yield Expr.Node bodyExpr
           yield closingBrace |]

    member val Name: Expr = nameExpr
    member val OpeningBrace: SingleTextNode = openingBrace
    member val Body: Expr = bodyExpr
    member val ClosingBrace: SingleTextNode = closingBrace

type ExprComputationNode(openingBrace: SingleTextNode, bodyExpr: Expr, closingBrace: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingBrace; yield Expr.Node bodyExpr; yield closingBrace |]

    member val OpeningBrace: SingleTextNode = openingBrace
    member val Body: Expr = bodyExpr
    member val ClosingBrace: SingleTextNode = closingBrace

type ExprLetOrUseNode(binding: BindingNode, inKeyword: SingleTextNode option, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield binding; yield! noa inKeyword |]
    member val Binding: BindingNode = binding
    member val In: SingleTextNode option = inKeyword

type ExprLetOrUseBangNode
    (leadingKeyword: SingleTextNode, pat: Pattern, equals: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield leadingKeyword
           yield Pattern.Node pat
           yield equals
           yield Expr.Node expr |]

    member val LeadingKeyword: SingleTextNode = leadingKeyword
    member val Pattern: Pattern = pat
    member val Equals: SingleTextNode = equals
    member val Expression: Expr = expr

type ExprAndBang(leadingKeyword: SingleTextNode, pat: Pattern, equals: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield leadingKeyword
           yield Pattern.Node pat
           yield equals
           yield Expr.Node expr |]

    member val LeadingKeyword: SingleTextNode = leadingKeyword
    member val Pattern: Pattern = pat
    member val Equals: SingleTextNode = equals
    member val Expression: Expr = expr

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ComputationExpressionStatement =
    | LetOrUseStatement of ExprLetOrUseNode
    | LetOrUseBangStatement of ExprLetOrUseBangNode
    | AndBangStatement of ExprAndBang
    | OtherStatement of Expr

    static member Node(ces: ComputationExpressionStatement) : Node =
        match ces with
        | LetOrUseStatement n -> n
        | LetOrUseBangStatement n -> n
        | AndBangStatement n -> n
        | OtherStatement o -> Expr.Node o

type ExprCompExprBodyNode(statements: ComputationExpressionStatement list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! List.map ComputationExpressionStatement.Node statements |]

    member val Statements: ComputationExpressionStatement list = statements

type ExprJoinInNode(lhs: Expr, inNode: SingleTextNode, rhs: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node lhs; yield inNode; yield Expr.Node rhs |]
    member val LeftHandSide: Expr = lhs
    member val In: SingleTextNode = inNode
    member val RightHandSide: Expr = rhs

type ExprParenLambdaNode
    (openingParen: SingleTextNode, lambda: ExprLambdaNode, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield lambda; yield closingParen |]
    member val OpeningParen: SingleTextNode = openingParen
    member val Lambda: ExprLambdaNode = lambda
    member val ClosingParen: SingleTextNode = closingParen

type ExprLambdaNode(funNode: SingleTextNode, parameters: Pattern list, arrow: SingleTextNode, expr: Expr, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield funNode
           yield! List.map Pattern.Node parameters
           yield arrow
           yield Expr.Node expr |]

    member val Fun: SingleTextNode = funNode
    member val Parameters: Pattern list = parameters
    member val Arrow: SingleTextNode = arrow
    member val Expr: Expr = expr

type MatchClauseNode
    (
        bar: SingleTextNode option,
        pattern: Pattern,
        whenExpr: Expr option,
        arrow: SingleTextNode,
        bodyExpr: Expr,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa bar
           yield Pattern.Node pattern
           yield! noa (Option.map Expr.Node whenExpr)
           yield arrow
           yield Expr.Node bodyExpr |]

    member val Bar: SingleTextNode option = bar
    member val Pattern: Pattern = pattern
    member val WhenExpr: Expr option = whenExpr
    member val Arrow: SingleTextNode = arrow
    member val BodyExpr: Expr = bodyExpr

type ExprMatchLambdaNode(functionNode: SingleTextNode, clauses: MatchClauseNode list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield functionNode; yield! nodes clauses |]
    member val Function: SingleTextNode = functionNode
    member val Clauses: MatchClauseNode list = clauses

type ExprMatchNode
    (matchNode: SingleTextNode, matchExpr: Expr, withNode: SingleTextNode, clauses: MatchClauseNode list, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield matchNode
           yield Expr.Node matchExpr
           yield withNode
           yield! nodes clauses |]

    member val Match: SingleTextNode = matchNode
    member val MatchExpr: Expr = matchExpr
    member val With: SingleTextNode = withNode
    member val Clauses: MatchClauseNode list = clauses

type ExprTraitCallNode(t: Type, md: MemberDefn, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node t; yield MemberDefn.Node md; yield Expr.Node expr |]

    member val Type: Type = t
    member val MemberDefn: MemberDefn = md
    member val Expr: Expr = expr

type ExprParenFunctionNameWithStarNode
    (openingParen: SingleTextNode, functionName: SingleTextNode, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield functionName; yield closingParen |]
    member val OpeningParen: SingleTextNode = openingParen
    member val FunctionName: SingleTextNode = functionName
    member val ClosingParen: SingleTextNode = closingParen

type ExprParenNode(openingParen: SingleTextNode, expr: Expr, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield Expr.Node expr; yield closingParen |]

    member val OpeningParen: SingleTextNode = openingParen
    member val Expr: Expr = expr
    member val ClosingParen: SingleTextNode = closingParen

type ExprDynamicNode(funcExpr: Expr, argExpr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node funcExpr; yield Expr.Node argExpr |]
    member val FuncExpr: Expr = funcExpr
    member val ArgExpr: Expr = argExpr

type ExprPrefixAppNode(operator: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield operator; yield Expr.Node expr |]
    member val Operator: SingleTextNode = operator
    member val Expr: Expr = expr

type InfixApp =
    interface
    end

type ExprSameInfixAppsNode(leadingExpr: Expr, subsequentExpressions: (SingleTextNode * Expr) list, range: range) =
    inherit NodeBase(range)
    interface InfixApp

    override val Children: Node array =
        let xs =
            List.collect (fun (operator, expr) -> [ (operator :> Node); Expr.Node expr ]) subsequentExpressions

        [| yield Expr.Node leadingExpr; yield! xs |]

    member val LeadingExpr: Expr = leadingExpr
    member val SubsequentExpressions: (SingleTextNode * Expr) list = subsequentExpressions

type ExprInfixAppNode(lhs: Expr, operator: SingleTextNode, rhs: Expr, range: range) =
    inherit NodeBase(range)
    interface InfixApp

    override val Children: Node array = [| yield Expr.Node lhs; yield operator; yield Expr.Node rhs |]
    member val LeftHandSide: Expr = lhs
    member val RightHandSide: Expr = rhs
    member val Operator: SingleTextNode = operator

type ExprIndexWithoutDotNode(identifierExpr: Expr, indexExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node identifierExpr; yield Expr.Node indexExpr |]
    member val Identifier: Expr = identifierExpr
    member val Index: Expr = indexExpr

type LinkSingleAppParen(functionName: Expr, parenExpr: ExprParenNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node functionName; yield parenExpr |]
    member val FunctionName: Expr = functionName
    member val Paren: ExprParenNode = parenExpr

type LinkSingleAppUnit(functionName: Expr, unit: UnitNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node functionName; yield unit |]
    member val FunctionName: Expr = functionName
    member val Unit: UnitNode = unit

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ChainLink =
    | Identifier of Expr
    | Dot of SingleTextNode
    | Expr of Expr
    | AppParen of
        // There should only be one argument
        LinkSingleAppParen
    | AppUnit of LinkSingleAppUnit
    // [ expr ] from DotIndexedGet
    | IndexExpr of Expr // e.[f]

    static member Node(link: ChainLink) : Node =
        match link with
        | Identifier e -> Expr.Node e
        | Dot n -> n
        | Expr e -> Expr.Node e
        | AppParen n -> n
        | AppUnit n -> n
        | IndexExpr e -> Expr.Node e

type ExprChain(links: ChainLink list, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = List.map ChainLink.Node links |> List.toArray
    member val Links: ChainLink list = links

type ExprAppLongIdentAndSingleParenArgNode(functionName: IdentListNode, argExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield functionName; yield Expr.Node argExpr |]
    member val FunctionName: IdentListNode = functionName
    member val ArgExpr: Expr = argExpr

type ExprAppSingleParenArgNode(functionExpr: Expr, argExpr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node functionExpr; yield Expr.Node argExpr |]
    member val FunctionExpr: Expr = functionExpr
    member val ArgExpr: Expr = argExpr

type ExprAppWithLambdaNode
    (
        functionName: Expr,
        arguments: Expr list,
        openingParen: SingleTextNode,
        lambda: Choice<ExprLambdaNode, ExprMatchLambdaNode>,
        closingParen: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        let lambdaNode =
            match lambda with
            | Choice1Of2 n -> n :> Node
            | Choice2Of2 n -> n

        [| yield Expr.Node functionName
           yield! List.map Expr.Node arguments
           yield openingParen
           yield lambdaNode
           yield closingParen |]

    member val FunctionName: Expr = functionName
    member val Arguments: Expr list = arguments
    member val OpeningParen: SingleTextNode = openingParen
    member val Lambda: Choice<ExprLambdaNode, ExprMatchLambdaNode> = lambda
    member val ClosingParen: SingleTextNode = closingParen

type ExprNestedIndexWithoutDotNode(identifierExpr: Expr, indexExpr: Expr, argumentExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node identifierExpr
           yield Expr.Node indexExpr
           yield Expr.Node argumentExpr |]

    member val Identifier: Expr = identifierExpr
    member val Index: Expr = indexExpr
    member val Argument: Expr = argumentExpr

type ExprAppNode(functionExpr: Expr, arguments: Expr list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node functionExpr; yield! List.map Expr.Node arguments |]

    member val FunctionExpr: Expr = functionExpr
    member val Arguments: Expr list = arguments

type ExprTypeAppNode
    (
        identifierExpr: Expr,
        lessThan: SingleTextNode,
        typeParameters: Type list,
        greaterThan: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node identifierExpr
           yield lessThan
           yield! List.map Type.Node typeParameters
           yield greaterThan |]

    member val Identifier: Expr = identifierExpr
    member val LessThan: SingleTextNode = lessThan
    member val TypeParameters: Type list = typeParameters
    member val GreaterThan: SingleTextNode = greaterThan

type ExprTryWithSingleClauseNode
    (tryNode: SingleTextNode, tryExpr: Expr, withNode: SingleTextNode, clause: MatchClauseNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield tryNode; yield Expr.Node tryExpr; yield withNode; yield clause |]

    member val Try: SingleTextNode = tryNode
    member val TryExpr: Expr = tryExpr
    member val With: SingleTextNode = withNode
    member val Clause: MatchClauseNode = clause

type ExprTryWithNode
    (tryNode: SingleTextNode, tryExpr: Expr, withNode: SingleTextNode, clauses: MatchClauseNode list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield tryNode
           yield Expr.Node tryExpr
           yield withNode
           yield! nodes clauses |]

    member val Try: SingleTextNode = tryNode
    member val TryExpr: Expr = tryExpr
    member val With: SingleTextNode = withNode
    member val Clauses: MatchClauseNode list = clauses

type ExprTryFinallyNode
    (tryNode: SingleTextNode, tryExpr: Expr, finallyNode: SingleTextNode, finallyExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield tryNode
           yield Expr.Node tryExpr
           yield finallyNode
           yield Expr.Node finallyExpr |]

    member val Try: SingleTextNode = tryNode
    member val TryExpr: Expr = tryExpr
    member val Finally: SingleTextNode = finallyNode
    member val FinallyExpr: Expr = finallyExpr

type ElseIfNode(mElse: range, mIf: range, condition: Node, range: range) as elseIfNode =
    let mutable elseCursor = None
    let mutable ifCursor = None
    let nodesBefore = Queue<TriviaNode>(0)
    let nodesAfter = Queue<TriviaNode>(0)
    let mutable lastNodeAfterIsLineCommentAfterSource = false

    let elseNode =
        { new Node with
            member _.ContentBefore: TriviaNode seq = Seq.empty
            member _.HasContentBefore: bool = false
            member _.ContentAfter: TriviaNode seq = Seq.empty
            member _.HasContentAfter: bool = false
            member _.Range = mElse

            member _.AddBefore(triviaNode: TriviaNode) =
                (elseIfNode :> Node).AddBefore triviaNode

            member _.AddAfter(triviaNode: TriviaNode) =
                (elseIfNode :> Node).AddAfter triviaNode

            member _.Children = Array.empty
            member _.AddCursor cursor = elseCursor <- Some cursor
            member _.TryGetCursor = elseCursor }

    let ifNode =
        { new Node with
            member _.ContentBefore: TriviaNode seq = Seq.empty
            member _.HasContentBefore: bool = false
            member _.ContentAfter: TriviaNode seq = Seq.empty
            member _.HasContentAfter: bool = false
            member _.Range = mIf

            member _.AddBefore(triviaNode: TriviaNode) =
                match triviaNode.Content with
                | CommentOnSingleLine _
                | Newline -> condition.AddBefore triviaNode
                | _ -> (elseIfNode :> Node).AddAfter triviaNode

            member _.AddAfter(triviaNode: TriviaNode) =
                (elseIfNode :> Node).AddAfter triviaNode

            member _.Children = Array.empty
            member _.AddCursor cursor = ifCursor <- Some cursor
            member _.TryGetCursor = ifCursor }

    interface Node with
        member _.ContentBefore: TriviaNode seq = nodesBefore
        member _.HasContentBefore: bool = not (Seq.isEmpty nodesBefore)
        member _.ContentAfter: TriviaNode seq = nodesAfter
        member _.HasContentAfter: bool = not (Seq.isEmpty nodesAfter)
        member _.Range = range
        member _.AddBefore(triviaNode: TriviaNode) = nodesBefore.Enqueue triviaNode

        member _.AddAfter(triviaNode: TriviaNode) =
            match triviaNode.Content with
            | TriviaContent.LineCommentAfterSourceCode comment when lastNodeAfterIsLineCommentAfterSource ->
                // If we already have a line comment after the `else if`, we cannot add another one.
                // The next best thing would be to add it on the next line as content before of the condition.
                let triviaNode =
                    TriviaNode(TriviaContent.CommentOnSingleLine comment, triviaNode.Range)

                condition.AddBefore triviaNode
            | _ ->
                lastNodeAfterIsLineCommentAfterSource <-
                    match triviaNode.Content with
                    | LineCommentAfterSourceCode _ -> true
                    | _ -> false

                nodesAfter.Enqueue triviaNode

        member val Children = [| elseNode; ifNode |]
        member _.AddCursor _ = ()
        member _.TryGetCursor = None

[<RequireQualifiedAccess; NoComparison>]
type IfKeywordNode =
    | SingleWord of SingleTextNode
    | ElseIf of ElseIfNode

    member x.Node: Node =
        match x with
        | SingleWord n -> n :> Node
        | ElseIf n -> n :> Node

    member x.Range: range = x.Node.Range

type ExprIfThenNode(ifNode: IfKeywordNode, ifExpr: Expr, thenNode: SingleTextNode, thenExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield ifNode.Node
           yield Expr.Node ifExpr
           yield thenNode
           yield Expr.Node thenExpr |]

    member val If: IfKeywordNode = ifNode
    member val IfExpr: Expr = ifExpr
    member val Then: SingleTextNode = thenNode
    member val ThenExpr: Expr = thenExpr

type ExprIfThenElseNode
    (
        ifNode: IfKeywordNode,
        ifExpr: Expr,
        thenNode: SingleTextNode,
        thenExpr: Expr,
        elseNode: SingleTextNode,
        elseExpr: Expr,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield ifNode.Node
           yield Expr.Node ifExpr
           yield thenNode
           yield Expr.Node thenExpr
           yield elseNode
           yield Expr.Node elseExpr |]

    member val If: IfKeywordNode = ifNode
    member val IfExpr: Expr = ifExpr
    member val Then: SingleTextNode = thenNode
    member val ThenExpr: Expr = thenExpr
    member val Else: SingleTextNode = elseNode
    member val ElseExpr: Expr = elseExpr

type ExprIfThenElifNode(branches: ExprIfThenNode list, elseBranch: (SingleTextNode * Expr) option, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        let elseNodes =
            match elseBranch with
            | None -> []
            | Some(elseNode, elseExpr) -> [ yield (elseNode :> Node); yield Expr.Node elseExpr ]

        [| yield! nodes branches; yield! elseNodes |]

    member val Branches: ExprIfThenNode list = branches
    member val Else: (SingleTextNode * Expr) option = elseBranch

type ExprOptVarNode(isOptional: bool, identifier: IdentListNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield identifier |]
    member val IsOptional: bool = isOptional
    member val Identifier: IdentListNode = identifier

type ExprLongIdentSetNode(identifier: IdentListNode, rhs: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield identifier; yield Expr.Node rhs |]
    member val Identifier: IdentListNode = identifier
    member val Expr: Expr = rhs

type ExprDotIndexedGetNode(objectExpr: Expr, indexExpr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node objectExpr; yield Expr.Node indexExpr |]
    member val ObjectExpr: Expr = objectExpr
    member val IndexExpr: Expr = indexExpr

type ExprDotIndexedSetNode(objectExpr: Expr, indexExpr: Expr, valueExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node objectExpr
           yield Expr.Node indexExpr
           yield Expr.Node valueExpr |]

    member val ObjectExpr: Expr = objectExpr
    member val Index: Expr = indexExpr
    member val Value: Expr = valueExpr

type ExprNamedIndexedPropertySetNode(identifier: IdentListNode, indexExpr: Expr, valueExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield identifier; yield Expr.Node indexExpr; yield Expr.Node valueExpr |]

    member val Identifier: IdentListNode = identifier
    member val Index: Expr = indexExpr
    member val Value: Expr = valueExpr

type ExprDotNamedIndexedPropertySetNode
    (identifierExpr: Expr, name: IdentListNode, propertyExpr: Expr, setExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node identifierExpr
           yield name
           yield Expr.Node propertyExpr
           yield Expr.Node setExpr |]

    member val Identifier: Expr = identifierExpr
    member val Name: IdentListNode = name
    member val Property: Expr = propertyExpr
    member val Set: Expr = setExpr

type ExprSetNode(identifier: Expr, setExpr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node identifier; yield Expr.Node setExpr |]
    member val Identifier: Expr = identifier
    member val Set: Expr = setExpr

type StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode(typar: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typar; yield Type.Node t |]
    member val TypeParameter: SingleTextNode = typar
    member val Type: Type = t

[<NoComparison>]
type StaticOptimizationConstraint =
    | WhenTyparTyconEqualsTycon of StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode
    | WhenTyparIsStruct of SingleTextNode

    static member Node(c: StaticOptimizationConstraint) : Node =
        match c with
        | WhenTyparTyconEqualsTycon n -> n
        | WhenTyparIsStruct n -> n

type ExprLibraryOnlyStaticOptimizationNode
    (optimizedExpr: Expr, constraints: StaticOptimizationConstraint list, expr: Expr, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node optimizedExpr
           yield! List.map StaticOptimizationConstraint.Node constraints
           yield Expr.Node expr |]

    member val OptimizedExpr: Expr = optimizedExpr
    member val Constraints: StaticOptimizationConstraint list = constraints
    member val Expr: Expr = expr

type FillExprNode(expr: Expr, ident: SingleTextNode option, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node expr; yield! noa ident |]
    member val Expr: Expr = expr
    member val Ident: SingleTextNode option = ident

type ExprInterpolatedStringExprNode(parts: Choice<SingleTextNode, FillExprNode> list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield!
               List.map
                   (function
                   | Choice1Of2 n -> (n :> Node)
                   | Choice2Of2 n -> (n :> Node))
                   parts |]

    member val Parts: Choice<SingleTextNode, FillExprNode> list = parts

type ExprTripleNumberIndexRangeNode
    (
        startNode: SingleTextNode,
        startDots: SingleTextNode,
        centerNode: SingleTextNode,
        endDots: SingleTextNode,
        endNode: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield startNode
           yield startDots
           yield centerNode
           yield endDots
           yield endNode |]

    member val Start: SingleTextNode = startNode
    member val StartDots: SingleTextNode = startDots
    member val Center: SingleTextNode = centerNode
    member val EndDots: SingleTextNode = endDots
    member val End: SingleTextNode = endNode

type ExprIndexRangeNode(fromExpr: Expr option, dots: SingleTextNode, toExpr: Expr option, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa (Option.map Expr.Node fromExpr)
           yield dots
           yield! noa (Option.map Expr.Node toExpr) |]

    member val From: Expr option = fromExpr
    member val Dots: SingleTextNode = dots
    member val To: Expr option = toExpr

type ExprIndexFromEndNode(expr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| Expr.Node expr |]
    member val Expr: Expr = expr

type ExprDotLambda(underscore: SingleTextNode, dot: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| underscore; dot; Expr.Node expr |]
    member val Underscore: SingleTextNode = underscore
    member val Dot: SingleTextNode = dot
    member val Expr: Expr = expr

type ExprBeginEndNode(beginNode: SingleTextNode, expr: Expr, endNode: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield beginNode; yield Expr.Node expr; yield endNode |]

    member val Begin: SingleTextNode = beginNode
    member val Expr: Expr = expr
    member val End: SingleTextNode = endNode

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
    | Lazy of ExprLazyNode
    | Single of ExprSingleNode
    | Constant of Constant
    | Null of SingleTextNode
    | Quote of ExprQuoteNode
    | Typed of ExprTypedNode
    | New of ExprNewNode
    | Tuple of ExprTupleNode
    | StructTuple of ExprStructTupleNode
    | ArrayOrList of ExprArrayOrListNode
    | Record of ExprRecordNode
    | InheritRecord of ExprInheritRecordNode
    | AnonStructRecord of ExprAnonStructRecordNode
    | ObjExpr of ExprObjExprNode
    | While of ExprWhileNode
    | For of ExprForNode
    | ForEach of ExprForEachNode
    | NamedComputation of ExprNamedComputationNode
    | Computation of ExprComputationNode
    | CompExprBody of ExprCompExprBodyNode
    | JoinIn of ExprJoinInNode
    | ParenLambda of ExprParenLambdaNode
    | Lambda of ExprLambdaNode
    | MatchLambda of ExprMatchLambdaNode
    | Match of ExprMatchNode
    | TraitCall of ExprTraitCallNode
    | ParenILEmbedded of SingleTextNode
    | ParenFunctionNameWithStar of ExprParenFunctionNameWithStarNode
    | Paren of ExprParenNode
    | Dynamic of ExprDynamicNode
    | PrefixApp of ExprPrefixAppNode
    | SameInfixApps of ExprSameInfixAppsNode
    | InfixApp of ExprInfixAppNode
    | IndexWithoutDot of ExprIndexWithoutDotNode
    | AppLongIdentAndSingleParenArg of ExprAppLongIdentAndSingleParenArgNode
    | AppSingleParenArg of ExprAppSingleParenArgNode
    | AppWithLambda of ExprAppWithLambdaNode
    | NestedIndexWithoutDot of ExprNestedIndexWithoutDotNode
    | App of ExprAppNode
    | TypeApp of ExprTypeAppNode
    | TryWithSingleClause of ExprTryWithSingleClauseNode
    | TryWith of ExprTryWithNode
    | TryFinally of ExprTryFinallyNode
    | IfThen of ExprIfThenNode
    | IfThenElse of ExprIfThenElseNode
    | IfThenElif of ExprIfThenElifNode
    | Ident of SingleTextNode
    | OptVar of ExprOptVarNode
    | LongIdentSet of ExprLongIdentSetNode
    | DotIndexedGet of ExprDotIndexedGetNode
    | DotIndexedSet of ExprDotIndexedSetNode
    | NamedIndexedPropertySet of ExprNamedIndexedPropertySetNode
    | DotNamedIndexedPropertySet of ExprDotNamedIndexedPropertySetNode
    | Set of ExprSetNode
    | LibraryOnlyStaticOptimization of ExprLibraryOnlyStaticOptimizationNode
    | InterpolatedStringExpr of ExprInterpolatedStringExprNode
    | IndexRangeWildcard of SingleTextNode
    | TripleNumberIndexRange of ExprTripleNumberIndexRangeNode
    | IndexRange of ExprIndexRangeNode
    | IndexFromEnd of ExprIndexFromEndNode
    | Typar of SingleTextNode
    | Chain of ExprChain
    | DotLambda of ExprDotLambda
    | BeginEnd of ExprBeginEndNode

    static member Node(x: Expr) : Node =
        match x with
        | Lazy n -> n
        | Single n -> n
        | Constant n -> Constant.Node n
        | Null n -> n
        | Quote n -> n
        | Typed n -> n
        | New n -> n
        | Tuple n -> n
        | StructTuple n -> n
        | ArrayOrList n -> n
        | Record n -> n
        | InheritRecord n -> n
        | AnonStructRecord n -> n
        | ObjExpr n -> n
        | While n -> n
        | For n -> n
        | ForEach n -> n
        | NamedComputation n -> n
        | Computation n -> n
        | CompExprBody n -> n
        | JoinIn n -> n
        | ParenLambda n -> n
        | Lambda n -> n
        | MatchLambda n -> n
        | Match n -> n
        | TraitCall n -> n
        | ParenILEmbedded n -> n
        | ParenFunctionNameWithStar n -> n
        | Paren n -> n
        | Dynamic n -> n
        | PrefixApp n -> n
        | SameInfixApps n -> n
        | InfixApp n -> n
        | IndexWithoutDot n -> n
        | AppLongIdentAndSingleParenArg n -> n
        | AppSingleParenArg n -> n
        | AppWithLambda n -> n
        | NestedIndexWithoutDot n -> n
        | App n -> n
        | TypeApp n -> n
        | TryWithSingleClause n -> n
        | TryWith n -> n
        | TryFinally n -> n
        | IfThen n -> n
        | IfThenElse n -> n
        | IfThenElif n -> n
        | Ident n -> n
        | OptVar n -> n
        | LongIdentSet n -> n
        | DotIndexedGet n -> n
        | DotIndexedSet n -> n
        | NamedIndexedPropertySet n -> n
        | DotNamedIndexedPropertySet n -> n
        | Set n -> n
        | LibraryOnlyStaticOptimization n -> n
        | InterpolatedStringExpr n -> n
        | IndexRangeWildcard n -> n
        | TripleNumberIndexRange n -> n
        | IndexRange n -> n
        | IndexFromEnd n -> n
        | Typar n -> n
        | Chain n -> n
        | DotLambda n -> n
        | BeginEnd n -> n

    member e.HasParentheses: bool =
        match e with
        | Expr.Paren _ -> true
        | _ -> false

type OpenModuleOrNamespaceNode(identListNode: IdentListNode, range: range) =
    inherit NodeBase(range)

    override val Children: SyntaxOak.Node array = Array.empty
    member val Name: IdentListNode = identListNode

type OpenTargetNode(target: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node target |]
    member val Target: Type = target

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Open =
    | ModuleOrNamespace of OpenModuleOrNamespaceNode
    | Target of OpenTargetNode

    static member Node(x: Open) : Node =
        match x with
        | ModuleOrNamespace n -> n
        | Target n -> n

type OpenListNode(opens: Open list) =
    inherit NodeBase(List.map (Open.Node >> nodeRange) opens |> combineRanges)

    override val Children: Node array = [| yield! (List.map Open.Node opens) |]
    member val Opens: Open list = opens

type HashDirectiveListNode(hashDirectives: ParsedHashDirectiveNode list) =
    inherit NodeBase(hashDirectives |> List.map (fun n -> n.Range) |> combineRanges)

    override val Children: Node array = [| yield! nodes hashDirectives |]
    member val HashDirectives: ParsedHashDirectiveNode list = hashDirectives

type AttributeNode(typeName: IdentListNode, expr: Expr option, target: SingleTextNode option, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeName; yield! noa (Option.map Expr.Node expr); yield! noa target |]

    member val TypeName: IdentListNode = typeName
    member val Expr: Expr option = expr
    member val Target: SingleTextNode option = target

/// The content from [< to >]
type AttributeListNode
    (openingToken: SingleTextNode, attributesNodes: AttributeNode list, closingToken: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingToken; yield! nodes attributesNodes; yield closingToken |]

    member val Opening: SingleTextNode = openingToken
    member val Attributes: AttributeNode list = attributesNodes
    member val Closing: SingleTextNode = closingToken

type MultipleAttributeListNode(attributeLists: AttributeListNode list, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! nodes attributeLists |]
    member val AttributeLists: AttributeListNode list = attributeLists
    member val IsEmpty: bool = attributeLists.IsEmpty

type ModuleDeclAttributesNode(attributes: MultipleAttributeListNode option, doExpr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! noa attributes; yield Expr.Node doExpr |]
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Expr: Expr = doExpr

type ExceptionDefnNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        unionCase: UnionCaseNode,
        withKeyword: SingleTextNode option,
        ms: MemberDefn list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa accessibility
           yield unionCase
           yield! noa withKeyword
           yield! nodes (List.map MemberDefn.Node ms) |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Accessibility: SingleTextNode option = accessibility
    member val UnionCase: UnionCaseNode = unionCase
    member val WithKeyword: SingleTextNode option = withKeyword
    member val Members: MemberDefn list = ms

type ExternBindingPatternNode
    (attributes: MultipleAttributeListNode option, t: Type option, pat: Pattern option, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield! noa (Option.map Type.Node t)
           yield! noa (Option.map Pattern.Node pat) |]

    member val Attributes: MultipleAttributeListNode option = attributes
    member val Type: Type option = t
    member val Pattern: Pattern option = pat

type ExternBindingNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        externNode: SingleTextNode,
        attributesOfType: MultipleAttributeListNode option,
        t: Type,
        accessibility: SingleTextNode option,
        identifier: IdentListNode,
        openingParen: SingleTextNode,
        parameters: ExternBindingPatternNode list,
        closingParen: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield externNode
           yield! noa attributesOfType
           yield Type.Node t
           yield! noa accessibility
           yield identifier
           yield openingParen
           yield! nodes parameters
           yield closingParen |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Extern: SingleTextNode = externNode
    member val AttributesOfType: MultipleAttributeListNode option = attributesOfType
    member val Type: Type = t
    member val Accessibility: SingleTextNode option = accessibility
    member val Identifier: IdentListNode = identifier
    member val OpeningParen: SingleTextNode = openingParen
    member val Parameters: ExternBindingPatternNode list = parameters
    member val ClosingParen: SingleTextNode = closingParen

type ModuleAbbrevNode(moduleNode: SingleTextNode, name: SingleTextNode, alias: IdentListNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield moduleNode; yield name; yield alias |]
    member val Module: SingleTextNode = moduleNode
    member val Name: SingleTextNode = name
    member val Alias: IdentListNode = alias

type NestedModuleNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        moduleKeyword: SingleTextNode,
        accessibility: SingleTextNode option,
        isRecursive: bool,
        identifier: IdentListNode,
        equalsNode: SingleTextNode,
        decls: ModuleDecl list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield moduleKeyword
           yield! noa accessibility
           yield identifier
           yield equalsNode
           yield! List.map ModuleDecl.Node decls |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Module: SingleTextNode = moduleKeyword
    member val Accessibility: SingleTextNode option = accessibility
    member val IsRecursive: bool = isRecursive
    member val Identifier: IdentListNode = identifier
    member val Equals: SingleTextNode = equalsNode
    member val Declarations: ModuleDecl list = decls

/// Each case in this DU should have a container node
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ModuleDecl =
    | OpenList of OpenListNode
    | HashDirectiveList of HashDirectiveListNode
    | Attributes of ModuleDeclAttributesNode
    | DeclExpr of Expr
    | Exception of ExceptionDefnNode
    | ExternBinding of ExternBindingNode
    | TopLevelBinding of BindingNode
    | ModuleAbbrev of ModuleAbbrevNode
    | NestedModule of NestedModuleNode
    | TypeDefn of TypeDefn
    | Val of ValNode

    static member Node(x: ModuleDecl) : Node =
        match x with
        | OpenList n -> n
        | HashDirectiveList n -> n
        | Attributes n -> n
        | DeclExpr e -> Expr.Node e
        | Exception n -> n
        | ExternBinding n -> n
        | TopLevelBinding n -> n
        | ModuleAbbrev n -> n
        | NestedModule n -> n
        | TypeDefn t -> TypeDefn.Node t
        | Val n -> n

type BindingReturnInfoNode(colon: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield colon; yield Type.Node t |]
    member val Colon: SingleTextNode = colon
    member val Type: Type = t

type BindingNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        isMutable: bool,
        inlineNode: SingleTextNode option,
        accessibility: SingleTextNode option,
        functionName: Choice<IdentListNode, Pattern>,
        genericTypeParameters: TyparDecls option,
        parameters: Pattern list,
        returnType: BindingReturnInfoNode option,
        equals: SingleTextNode,
        expr: Expr,
        range: range
    ) =
    inherit NodeBase(range)
    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode = leadingKeyword
    member val IsMutable: bool = isMutable
    member val Inline: SingleTextNode option = inlineNode
    member val Accessibility: SingleTextNode option = accessibility
    member val FunctionName: Choice<IdentListNode, Pattern> = functionName
    member val GenericTypeParameters: TyparDecls option = genericTypeParameters
    member val Parameters: Pattern list = parameters
    member val ReturnType: BindingReturnInfoNode option = returnType
    member val Equals: SingleTextNode = equals
    member val Expr: Expr = expr

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa inlineNode
           yield! noa accessibility
           yield
               match functionName with
               | Choice1Of2 n -> (n :> Node)
               | Choice2Of2 p -> Pattern.Node p
           yield! noa (Option.map TyparDecls.Node genericTypeParameters)
           yield! nodes (List.map Pattern.Node parameters)
           yield! noa returnType
           yield equals
           yield Expr.Node expr |]

type BindingListNode(bindings: BindingNode list, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! nodes bindings |]
    member val Bindings: BindingNode list = bindings

type FieldNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode option,
        mutableKeyword: SingleTextNode option,
        accessibility: SingleTextNode option,
        name: SingleTextNode option,
        t: Type,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa leadingKeyword
           yield! noa mutableKeyword
           yield! noa accessibility
           yield! noa name
           yield Type.Node t |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode option = leadingKeyword
    member val MutableKeyword: SingleTextNode option = mutableKeyword
    member val Accessibility: SingleTextNode option = accessibility
    member val Name: SingleTextNode option = name
    member val Type: Type = t

type UnionCaseNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        bar: SingleTextNode option,
        identifier: SingleTextNode,
        fields: FieldNode list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa bar
           yield identifier
           yield! nodes fields |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Bar: SingleTextNode option = bar
    member val Identifier: SingleTextNode = identifier
    member val Fields: FieldNode list = fields

type TypeNameNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: SingleTextNode,
        ao: SingleTextNode option,
        identifier: IdentListNode,
        typeParams: TyparDecls option,
        constraints: TypeConstraint list,
        implicitConstructor: ImplicitConstructorNode option,
        equalsToken: SingleTextNode option,
        withKeyword: SingleTextNode option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa ao
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield! List.map TypeConstraint.Node constraints
           yield! noa implicitConstructor
           yield! noa equalsToken
           yield! noa withKeyword |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val IsFirstType: bool = leadingKeyword.Text = "type"
    member val LeadingKeyword: SingleTextNode = leadingKeyword
    member val Accessibility: SingleTextNode option = ao
    member val Identifier: IdentListNode = identifier
    member val TypeParameters: TyparDecls option = typeParams
    member val Constraints: TypeConstraint list = constraints
    member val ImplicitConstructor: ImplicitConstructorNode option = implicitConstructor
    member val EqualsToken: SingleTextNode option = equalsToken
    member val WithKeyword: SingleTextNode option = withKeyword

type ITypeDefn =
    abstract member TypeName: TypeNameNode
    abstract member Members: MemberDefn list

type EnumCaseNode
    (
        xmlDoc: XmlDocNode option,
        bar: SingleTextNode option,
        attributes: MultipleAttributeListNode option,
        identifier: SingleTextNode,
        equals: SingleTextNode,
        constant: Expr,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa bar
           yield identifier
           yield equals
           yield Expr.Node constant |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Bar: SingleTextNode option = bar
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Identifier: SingleTextNode = identifier
    member val Equals: SingleTextNode = equals
    member val Constant: Expr = constant

type TypeDefnEnumNode(typeNameNode: TypeNameNode, enumCases: EnumCaseNode list, members: MemberDefn list, range: range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield! nodes enumCases
           yield! nodes (List.map MemberDefn.Node members) |]

    member val EnumCases: EnumCaseNode list = enumCases

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnUnionNode
    (
        typeNameNode: TypeNameNode,
        accessibility: SingleTextNode option,
        unionCases: UnionCaseNode list,
        members: MemberDefn list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield! noa accessibility
           yield! nodes unionCases
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Accessibility: SingleTextNode option = accessibility
    member val UnionCases: UnionCaseNode list = unionCases

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnRecordNode
    (
        typeNameNode: TypeNameNode,
        accessibility: SingleTextNode option,
        openingBrace: SingleTextNode,
        fields: FieldNode list,
        closingBrace: SingleTextNode,
        members: MemberDefn list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield! noa accessibility
           yield openingBrace
           yield! nodes fields
           yield closingBrace
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Accessibility: SingleTextNode option = accessibility
    member val OpeningBrace: SingleTextNode = openingBrace
    member val Fields: FieldNode list = fields
    member val ClosingBrace: SingleTextNode = closingBrace

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnAbbrevNode(typeNameNode: TypeNameNode, t: Type, members: MemberDefn list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield Type.Node t
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Type: Type = t

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type SimplePatNode
    (
        attributes: MultipleAttributeListNode option,
        isOptional: bool,
        identifier: SingleTextNode,
        t: Type option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield identifier
           yield! noa (Option.map Type.Node t) |]

    member val Attributes: MultipleAttributeListNode option = attributes
    member val IsOptional: bool = isOptional
    member val Identifier: SingleTextNode = identifier
    member val Type: Type option = t

type AsSelfIdentifierNode(asNode: SingleTextNode, self: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: SyntaxOak.Node array = [| yield (asNode :> Node); yield self |]
    member val As: SingleTextNode = asNode
    member val Self: SingleTextNode = self

type ImplicitConstructorNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        openingParen: SingleTextNode,
        items: Choice<SimplePatNode, SingleTextNode> list,
        closingParen: SingleTextNode,
        self: AsSelfIdentifierNode option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa accessibility
           yield openingParen
           for item in items do
               match item with
               | Choice1Of2 node -> yield node
               | Choice2Of2 comma -> yield comma
           yield closingParen
           yield! noa self |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Accessibility: SingleTextNode option = accessibility
    member val OpeningParen: SingleTextNode = openingParen
    member val Items: Choice<SimplePatNode, SingleTextNode> list = items
    member val ClosingParen: SingleTextNode = closingParen
    member val Self: AsSelfIdentifierNode option = self

type TypeDefnExplicitBodyNode(kind: SingleTextNode, members: MemberDefn list, endNode: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield kind; yield! nodes (List.map MemberDefn.Node members); yield endNode |]

    member val Kind: SingleTextNode = kind
    member val Members: MemberDefn list = members
    member val End: SingleTextNode = endNode

type TypeDefnExplicitNode
    (typeNameNode: TypeNameNode, body: TypeDefnExplicitBodyNode, members: MemberDefn list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield body
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Body: TypeDefnExplicitBodyNode = body

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnAugmentationNode(typeNameNode: TypeNameNode, members: MemberDefn list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeNameNode; yield! (List.map MemberDefn.Node members) |]

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnDelegateNode
    (typeNameNode: TypeNameNode, delegateNode: SingleTextNode, typeList: TypeFunsNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeNameNode; yield delegateNode; yield typeList |]

    member val DelegateNode: SingleTextNode = delegateNode
    member val TypeList: TypeFunsNode = typeList

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = List.empty

type TypeDefnRegularNode(typeNameNode: TypeNameNode, members: MemberDefn list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeNameNode; yield! List.map MemberDefn.Node members |]

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TypeDefn =
    | Enum of TypeDefnEnumNode
    | Union of TypeDefnUnionNode
    | Record of TypeDefnRecordNode
    | None of TypeNameNode
    | Abbrev of TypeDefnAbbrevNode
    | Explicit of TypeDefnExplicitNode
    | Augmentation of TypeDefnAugmentationNode
    | Delegate of TypeDefnDelegateNode
    | Regular of TypeDefnRegularNode

    static member Node(x: TypeDefn) : Node =
        match x with
        | Enum n -> n
        | Union n -> n
        | Record n -> n
        | None n -> n
        | Abbrev n -> n
        | Explicit n -> n
        | Augmentation n -> n
        | Delegate n -> n
        | Regular n -> n

    static member TypeDefnNode(x: TypeDefn) : ITypeDefn =
        match x with
        | Enum n -> n
        | Union n -> n
        | Record n -> n
        | None n ->
            { new ITypeDefn with
                member _.TypeName = n
                member _.Members = [] }
        | Abbrev n -> n
        | Explicit n -> n
        | Augmentation n -> n
        | Delegate n -> n
        | Regular n -> n

type MemberDefnInheritNode(inheritKeyword: SingleTextNode, baseType: Type, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node baseType |]

    member val Inherit: SingleTextNode = inheritKeyword
    member val BaseType: Type = baseType

type MemberDefnExplicitCtorNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        newKeyword: SingleTextNode,
        pat: Pattern,
        alias: SingleTextNode option,
        equals: SingleTextNode,
        expr: Expr,
        thenExpr: Expr option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa accessibility
           yield newKeyword
           yield Pattern.Node pat
           yield! noa alias
           yield equals
           yield Expr.Node expr
           yield! noa (Option.map Expr.Node thenExpr) |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val Accessibility: SingleTextNode option = accessibility
    member val New: SingleTextNode = newKeyword
    member val Pattern: Pattern = pat
    member val Alias: SingleTextNode option = alias
    member val Equals: SingleTextNode = equals
    member val Expr: Expr = expr
    member val ThenExpr: Expr option = thenExpr

type MemberDefnInterfaceNode
    (interfaceNode: SingleTextNode, t: Type, withNode: SingleTextNode option, members: MemberDefn list, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield interfaceNode
           yield Type.Node t
           yield! noa withNode
           yield! List.map MemberDefn.Node members |]

    member val Interface: SingleTextNode = interfaceNode
    member val Type: Type = t
    member val With: SingleTextNode option = withNode
    member val Members: MemberDefn list = members

type MemberDefnAutoPropertyNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        accessibility: SingleTextNode option,
        identifier: SingleTextNode,
        t: Type option,
        equals: SingleTextNode,
        expr: Expr,
        withGetSet: MultipleTextsNode option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa accessibility
           yield identifier
           yield! noa (Option.map Type.Node t)
           yield equals
           yield Expr.Node expr
           yield! noa withGetSet |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode = leadingKeyword
    member val Accessibility: SingleTextNode option = accessibility
    member val Identifier: SingleTextNode = identifier
    member val Type: Type option = t
    member val Equals: SingleTextNode = equals
    member val Expr: Expr = expr
    member val WithGetSet: MultipleTextsNode option = withGetSet

type MemberDefnAbstractSlotNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        identifier: SingleTextNode,
        typeParams: TyparDecls option,
        t: Type,
        withGetSet: MultipleTextsNode option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield Type.Node t
           yield! noa withGetSet |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode = leadingKeyword
    member val Identifier: SingleTextNode = identifier
    member val TypeParams: TyparDecls option = typeParams
    member val Type: Type = t
    member val WithGetSet: MultipleTextsNode option = withGetSet

type PropertyGetSetBindingNode
    (
        inlineNode: SingleTextNode option,
        accessibility: SingleTextNode option,
        leadingKeyword: SingleTextNode,
        parameters: Pattern list,
        returnType: BindingReturnInfoNode option,
        equals: SingleTextNode,
        expr: Expr,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa inlineNode
           yield! noa accessibility
           yield leadingKeyword
           yield! List.map Pattern.Node parameters
           yield! noa returnType
           yield equals
           yield Expr.Node expr |]

    member val Inline: SingleTextNode option = inlineNode
    member val Accessibility: SingleTextNode option = accessibility
    member val LeadingKeyword: SingleTextNode = leadingKeyword
    member val Parameters: Pattern list = parameters
    member val ReturnType: BindingReturnInfoNode option = returnType
    member val Equals: SingleTextNode = equals
    member val Expr: Expr = expr

type MemberDefnPropertyGetSetNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        inlineNode: SingleTextNode option,
        accessibility: SingleTextNode option,
        memberName: IdentListNode,
        withKeyword: SingleTextNode,
        firstBinding: PropertyGetSetBindingNode,
        andKeyword: SingleTextNode option,
        lastBinding: PropertyGetSetBindingNode option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa accessibility
           yield memberName
           yield withKeyword
           yield firstBinding
           yield! noa andKeyword
           yield! noa lastBinding |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode = leadingKeyword
    member val Inline: SingleTextNode option = inlineNode
    member val Accessibility: SingleTextNode option = accessibility
    member val MemberName: IdentListNode = memberName
    member val WithKeyword: SingleTextNode = withKeyword
    member val FirstBinding: PropertyGetSetBindingNode = firstBinding
    member val AndKeyword: SingleTextNode option = andKeyword
    member val LastBinding: PropertyGetSetBindingNode option = lastBinding

type ValNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode option,
        inlineNode: SingleTextNode option,
        isMutable: bool,
        accessibility: SingleTextNode option,
        identifier: SingleTextNode,
        typeParams: TyparDecls option,
        t: Type,
        equals: SingleTextNode option,
        eo: Expr option,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa leadingKeyword
           yield! noa accessibility
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield Type.Node t
           yield! noa equals
           yield! noa (Option.map Expr.Node eo) |]

    member val XmlDoc: XmlDocNode option = xmlDoc
    member val Attributes: MultipleAttributeListNode option = attributes
    member val LeadingKeyword: MultipleTextsNode option = leadingKeyword
    member val Inline: SingleTextNode option = inlineNode
    member val IsMutable: bool = isMutable
    member val Accessibility: SingleTextNode option = accessibility
    member val Identifier: SingleTextNode = identifier
    member val TypeParams: TyparDecls option = typeParams
    member val Type: Type = t
    member val Equals: SingleTextNode option = equals
    member val Expr: Expr option = eo

type MemberDefnSigMemberNode(valNode: ValNode, withGetSet: MultipleTextsNode option, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield valNode; yield! noa withGetSet |]
    member val Val: ValNode = valNode
    member val WithGetSet: MultipleTextsNode option = withGetSet

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MemberDefn =
    | ImplicitInherit of InheritConstructor
    | Inherit of MemberDefnInheritNode
    | ValField of FieldNode
    | Member of BindingNode
    | ExternBinding of ExternBindingNode
    | DoExpr of ExprSingleNode
    | LetBinding of BindingListNode
    | ExplicitCtor of MemberDefnExplicitCtorNode
    | Interface of MemberDefnInterfaceNode
    | AutoProperty of MemberDefnAutoPropertyNode
    | AbstractSlot of MemberDefnAbstractSlotNode
    | PropertyGetSet of MemberDefnPropertyGetSetNode
    | SigMember of MemberDefnSigMemberNode

    static member Node(md: MemberDefn) : Node =
        match md with
        | ImplicitInherit n -> InheritConstructor.Node n
        | Inherit n -> n
        | ValField n -> n
        | Member n -> n
        | ExternBinding n -> n
        | DoExpr n -> n
        | LetBinding n -> n
        | ExplicitCtor n -> n
        | Interface n -> n
        | AutoProperty n -> n
        | AbstractSlot n -> n
        | PropertyGetSet n -> n
        | SigMember n -> n

type UnitNode(openingParen: SingleTextNode, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield closingParen |]
    member val OpeningParen: SingleTextNode = openingParen
    member val ClosingParen: SingleTextNode = closingParen

type ConstantMeasureNode(constant: Constant, measure: UnitOfMeasureNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Constant.Node constant; yield measure |]
    member val Constant: Constant = constant
    member val Measure: UnitOfMeasureNode = measure

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Constant =
    | FromText of SingleTextNode
    | Unit of UnitNode
    | Measure of ConstantMeasureNode

    static member Node(c: Constant) : NodeBase =
        match c with
        | FromText n -> n
        | Unit n -> n
        | Measure n -> n

type TyparDeclNode
    (
        attributes: MultipleAttributeListNode option,
        typar: SingleTextNode,
        intersectionConstraints: Choice<Type, SingleTextNode> list,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield typar
           yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 amp -> amp :> Node)
                   intersectionConstraints |]

    member val Attributes: MultipleAttributeListNode option = attributes
    member val TypeParameter: SingleTextNode = typar
    member val IntersectionConstraints: Choice<Type, SingleTextNode> list = intersectionConstraints

type TyparDeclsPostfixListNode
    (
        lessThan: SingleTextNode,
        decls: TyparDeclNode list,
        constraints: TypeConstraint list,
        greaterThan: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield lessThan
           yield! nodes decls
           yield! List.map TypeConstraint.Node constraints
           yield greaterThan |]

    member val LessThan: SingleTextNode = lessThan
    member val Decls: TyparDeclNode list = decls
    member val Constraints: TypeConstraint list = constraints
    member val GreaterThan: SingleTextNode = greaterThan

type TyparDeclsPrefixListNode
    (openingParen: SingleTextNode, decls: TyparDeclNode list, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield! nodes decls; yield closingParen |]
    member val OpeningParen: SingleTextNode = openingParen
    member val Decls: TyparDeclNode list = decls
    member val ClosingParen: SingleTextNode = closingParen

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TyparDecls =
    | PostfixList of TyparDeclsPostfixListNode
    | PrefixList of TyparDeclsPrefixListNode
    | SinglePrefix of TyparDeclNode

    static member Node(td: TyparDecls) : Node =
        match td with
        | PostfixList n -> n
        | PrefixList n -> n
        | SinglePrefix n -> n

type TypeConstraintSingleNode(typar: SingleTextNode, kind: SingleTextNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield typar; yield kind |]
    member val Typar: SingleTextNode = typar
    member val Kind: SingleTextNode = kind

type TypeConstraintDefaultsToTypeNode(defaultNode: SingleTextNode, typar: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield defaultNode; yield typar; yield Type.Node t |]
    member val Default: SingleTextNode = defaultNode
    member val Typar: SingleTextNode = typar
    member val Type: Type = t

type TypeConstraintSubtypeOfTypeNode(typar: SingleTextNode, t: Type, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield typar; yield Type.Node t |]
    member val Typar: SingleTextNode = typar
    member val Type: Type = t

type TypeConstraintSupportsMemberNode(t: Type, memberSig: MemberDefn, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node t |]
    member val Type: Type = t
    member val MemberSig: MemberDefn = memberSig

type TypeConstraintEnumOrDelegateNode(typar: SingleTextNode, verb: string, ts: Type list, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield typar; yield! List.map Type.Node ts |]
    member val Typar: SingleTextNode = typar
    member val Verb: string = verb
    member val Types: Type list = ts

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TypeConstraint =
    | Single of TypeConstraintSingleNode
    | DefaultsToType of TypeConstraintDefaultsToTypeNode
    | SubtypeOfType of TypeConstraintSubtypeOfTypeNode
    | SupportsMember of TypeConstraintSupportsMemberNode
    | EnumOrDelegate of TypeConstraintEnumOrDelegateNode
    | WhereSelfConstrained of Type

    static member Node(tc: TypeConstraint) : Node =
        match tc with
        | Single n -> n
        | DefaultsToType n -> n
        | SubtypeOfType n -> n
        | SupportsMember n -> n
        | EnumOrDelegate n -> n
        | WhereSelfConstrained t -> Type.Node t

type UnitOfMeasureNode(lessThan: SingleTextNode, measure: Measure, greaterThan: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield lessThan; yield Measure.Node measure; yield greaterThan |]

    member val LessThan: SingleTextNode = lessThan
    member val Measure: Measure = measure
    member val GreaterThan: SingleTextNode = greaterThan

type MeasureOperatorNode(lhs: Measure, operator: SingleTextNode, rhs: Measure, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Measure.Node lhs; yield operator; yield Measure.Node rhs |]

    member val LeftHandSide: Measure = lhs
    member val Operator: SingleTextNode = operator
    member val RightHandSide: Measure = rhs

type MeasureDivideNode(lhs: Measure option, operator: SingleTextNode, rhs: Measure, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| match lhs with
           | Some n -> yield Measure.Node n
           | None -> ()
           yield operator
           yield Measure.Node rhs |]

    member val LeftHandSide: Measure option = lhs
    member val Operator: SingleTextNode = operator
    member val RightHandSide: Measure = rhs

type MeasurePowerNode(measure: Measure, caret: SingleTextNode, exponent: RationalConstNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Measure.Node measure
           yield caret
           yield RationalConstNode.Node exponent |]

    member val Measure: Measure = measure
    member val Caret: SingleTextNode = caret
    member val Exponent: RationalConstNode = exponent

type MeasureSequenceNode(measures: Measure list, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! List.map Measure.Node measures |]
    member val Measures: Measure list = measures

type MeasureParenNode(openingParen: SingleTextNode, measure: Measure, closingParen: SingleTextNode, range: range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield Measure.Node measure; yield closingParen |]

    member val OpeningParen: SingleTextNode = openingParen
    member val Measure: Measure = measure
    member val ClosingParen: SingleTextNode = closingParen

type RationalNode
    (
        openingParen: SingleTextNode,
        numerator: SingleTextNode,
        divOp: SingleTextNode,
        denominator: SingleTextNode,
        closingParen: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield openingParen
           yield numerator
           yield divOp
           yield denominator
           yield closingParen |]

    member val OpeningParen: SingleTextNode = openingParen
    member val Numerator: SingleTextNode = numerator
    member val DivOp: SingleTextNode = divOp
    member val Denominator: SingleTextNode = denominator
    member val ClosingParen: SingleTextNode = closingParen

type NegateRationalNode(minus: SingleTextNode, rationalConst: RationalConstNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield minus; yield RationalConstNode.Node rationalConst |]

    member val Minus: SingleTextNode = minus
    member val Rational: RationalConstNode = rationalConst

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type RationalConstNode =
    | Integer of SingleTextNode
    | Rational of RationalNode
    | Negate of NegateRationalNode

    static member Node(r: RationalConstNode) : Node =
        match r with
        | Integer n -> n
        | Rational n -> n
        | Negate n -> n

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Measure =
    | Single of SingleTextNode
    | Operator of MeasureOperatorNode
    | Divide of MeasureDivideNode
    | Power of MeasurePowerNode
    | Multiple of IdentListNode
    | Seq of MeasureSequenceNode
    | Paren of MeasureParenNode

    static member Node(m: Measure) : Node =
        match m with
        | Single n -> n
        | Operator n -> n
        | Divide n -> n
        | Power n -> n
        | Multiple n -> n
        | Seq n -> n
        | Paren n -> n
