module rec Fantomas.Core.SyntaxOak

open System.Collections.Generic
open FSharp.Compiler.Text

// Open questions:
// - Do we need to distinguish between SignatureFile and ImplementationFile?

type TriviaContent =
    | CommentOnSingleLine of string
    | LineCommentAfterSourceCode of comment: string
    | Newline

type TriviaNode(content: TriviaContent, range: range) =
    member x.Content = content
    member x.Range = range

[<Interface>]
type Node =
    abstract ContentBefore: TriviaNode seq
    abstract ContentAfter: TriviaNode seq
    abstract Range: range
    abstract Children: Node array
    abstract AddBefore: triviaNode: TriviaNode -> unit
    abstract AddAfter: triviaNode: TriviaNode -> unit

[<AbstractClass>]
type NodeBase(range: range) =
    let nodesBefore = Queue<TriviaNode>(0)
    let nodesAfter = Queue<TriviaNode>(0)

    abstract member Children: Node array

    interface Node with
        member _.ContentBefore: TriviaNode seq = nodesBefore
        member _.ContentAfter: TriviaNode seq = nodesAfter
        member _.Range = range
        member _.AddBefore(triviaNode: TriviaNode) = nodesBefore.Enqueue triviaNode
        member _.AddAfter(triviaNode: TriviaNode) = nodesAfter.Enqueue triviaNode
        member this.Children = this.Children

type StringNode(content: string, range: range) =
    inherit NodeBase(range)
    member x.Content = content
    override this.Children = Array.empty

let noa<'n when 'n :> Node> (n: 'n option) =
    match n with
    | None -> Array.empty
    | Some n -> [| n :> Node |]

let nodes<'n when 'n :> Node> (ns: seq<'n>) = Seq.cast<Node> ns

let nodeRange (n: Node) = n.Range

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
    | Ident of SingleTextNode
    | KnownDot of DotNode
    | UnknownDot

    member x.Range =
        match x with
        | Ident n -> Some (n :> Node).Range
        | KnownDot n -> Some (n :> Node).Range
        | UnknownDot -> None

type IdentListNode(content: IdentifierOrDot list, range) =
    inherit NodeBase(range)
    member x.IsEmpty = content.IsEmpty
    member x.Content = content
    static member Empty = IdentListNode(List.empty, Range.Zero)

    override x.Children =
        x.Content
        |> List.choose (function
            | IdentifierOrDot.Ident n -> Some(n :> Node)
            | _ -> None)
        |> Array.ofList

type SingleTextNode(idText: string, range: range) =
    inherit NodeBase(range)
    member _.Text = idText
    override x.Children = Array.empty

type MultipleTextsNode(content: SingleTextNode list, range) =
    inherit NodeBase(range)
    override x.Children = [| yield! nodes content |]
    member x.Content = content

type Oak(parsedHashDirectives: ParsedHashDirectiveNode list, modulesOrNamespaces: ModuleOrNamespaceNode list) =
    inherit
        NodeBase(
            [| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]
            |> Seq.map nodeRange
            |> combineRanges
        )

    member x.ParsedHashDirectives = parsedHashDirectives
    member x.ModulesOrNamespaces = modulesOrNamespaces

    override this.Children =
        [| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]

type ParsedHashDirectiveNode(ident: string, args: SingleTextNode list, range) =
    inherit NodeBase(range)
    member x.Ident = ident
    member x.Args = args
    override this.Children = [| yield! nodes args |]

type ModuleOrNamespaceNode(leadingKeyword: SingleTextNode option, name: IdentListNode, decls: ModuleDecl list, range) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.Name = name
    member x.Declarations = decls
    member x.IsNamed = Option.isSome x.LeadingKeyword

    override this.Children =
        [| yield! noa leadingKeyword
           if Option.isSome leadingKeyword then
               yield name
           yield! List.map ModuleDecl.Node decls |]

type TypeFunsNode(parameters: (Type * SingleTextNode) list, returnType: Type, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! nodes (List.collect (fun (t, arrow) -> [ yield Type.Node t; yield (arrow :> Node) ]) parameters)
           yield Type.Node returnType |]

    /// Type + arrow
    member x.Parameters = parameters
    member x.ReturnType = returnType

type TypeTupleNode(path: Choice<Type, SingleTextNode> list, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 n -> n :> Node)
                   path |]

    member x.Path = path

type TypeHashConstraintNode(hash: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield hash; yield Type.Node t |]
    member x.Hash = hash
    member x.Type = t

type TypeMeasurePowerNode(baseMeasure: Type, exponent: string, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Type.Node baseMeasure |]
    member x.BaseMeasure = baseMeasure
    member x.Exponent = exponent

type TypeStaticConstantExprNode(constNode: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield constNode; yield Expr.Node expr |]
    member x.Const = constNode
    member x.Expr = expr

type TypeStaticConstantNamedNode(identifier: Type, value: Type, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Type.Node identifier; yield Type.Node value |]
    member x.Identifier = identifier
    member x.Value = value

type TypeArrayNode(t: Type, rank: int, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Type.Node t |]
    member x.Type = t
    member x.Rank = rank

type TypeAppPostFixNode(first: Type, last: Type, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Type.Node first; yield Type.Node last |]
    member x.First = first
    member x.Last = last

type TypeAppPrefixNode
    (
        identifier: Type,
        postIdentifier: IdentListNode option,
        lessThan: SingleTextNode,
        arguments: Type list,
        greaterThan: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Type.Node identifier
           yield! noa postIdentifier
           yield lessThan
           yield! (List.map Type.Node arguments)
           yield greaterThan |]

    member x.Identifier = identifier
    member x.PostIdentifier = postIdentifier
    member x.GreaterThan = greaterThan
    member x.Arguments = arguments
    member x.LessThen = lessThan

type TypeStructTupleNode
    (
        keyword: SingleTextNode,
        path: Choice<Type, SingleTextNode> list,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield keyword
           yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 n -> n :> Node)
                   path
           yield closingParen |]

    member x.Keyword = keyword
    member x.Path = path
    member x.ClosingParen = closingParen

type TypeWithGlobalConstraintsNode(t: Type, constraints: TypeConstraint list, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Type.Node t; yield! List.map TypeConstraint.Node constraints |]

    member x.Type = t
    member x.TypeConstraints = constraints

type TypeAnonRecordNode
    (
        structNode: SingleTextNode option,
        openingToken: SingleTextNode option,
        fields: (SingleTextNode * Type) list,
        closingToken: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa structNode
           yield! noa openingToken
           yield! (fields |> List.collect (fun (i, t) -> [ yield (i :> Node); yield Type.Node t ]))
           yield closingToken |]

    member x.Struct = structNode
    member x.Opening = openingToken
    member x.Fields = fields
    member x.Closing = closingToken

type TypeParenNode(openingParen: SingleTextNode, t: Type, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override this.Children = [| yield openingParen; yield Type.Node t; yield closingParen |]
    member x.OpeningParen = openingParen
    member x.Type = t
    member x.ClosingParen = closingParen

type TypeSignatureParameterNode
    (
        attributes: MultipleAttributeListNode,
        identifier: SingleTextNode option,
        t: Type,
        range
    ) =
    inherit NodeBase(range)

    override this.Children = [| yield attributes; yield! noa identifier; yield Type.Node t |]

    member x.Attributes = attributes
    member x.Identifier = identifier
    member x.Type = t

type TypeOrNode(lhs: Type, orNode: SingleTextNode, rhs: Type, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Type.Node lhs; yield orNode; yield Type.Node rhs |]
    member x.LeftHandSide = lhs
    member x.Or = orNode
    member x.RightHandSide = rhs

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

type PatAttribNode(attrs: MultipleAttributeListNode, pat: Pattern, range) =
    inherit NodeBase(range)

    override this.Children = [| yield attrs; yield Pattern.Node pat |]

    member x.Attributes = attrs
    member x.Pattern = pat

/// A pattern composed from a left hand-side pattern, a single text token/operator and a right hand-side pattern.
type PatLeftMiddleRight(lhs: Pattern, middle: Choice<SingleTextNode, string>, rhs: Pattern, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Pattern.Node lhs
           match middle with
           | Choice1Of2 n -> yield n
           | _ -> ()
           yield Pattern.Node rhs |]

    member x.LeftHandSide = lhs
    member x.Middle = middle
    member x.RightHandSide = rhs

type PatAndsNode(pats: Pattern list, range) =
    inherit NodeBase(range)

    override this.Children = [| yield! List.map Pattern.Node pats |]
    member x.Patterns = pats

type PatTypedNode(pat: Pattern, t: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Pattern.Node pat; yield Type.Node t |]
    member x.Pattern = pat
    member x.Type = t

type PatNamedParenStarIdentNode
    (
        accessibility: SingleTextNode option,
        openingParen: SingleTextNode,
        name: SingleTextNode,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa accessibility
           yield openingParen
           yield name
           yield closingParen |]

    member x.Accessibility = accessibility
    member x.OpeningParen = openingParen
    member this.Name = name
    member x.ClosingParen = closingParen

type PatNamedNode(accessibility: SingleTextNode option, name: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield name |]
    member this.Name = name
    member x.Accessibility = accessibility

type NamePatPair(ident: SingleTextNode, equals: SingleTextNode, pat: Pattern, range) =
    inherit NodeBase(range)
    override this.Children = [| yield ident; yield equals; yield Pattern.Node pat |]
    member x.Ident = ident
    member x.Equals = equals
    member x.Pattern = pat

type PatNamePatPairsNode
    (
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        openingParen: SingleTextNode,
        pairs: NamePatPair list,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield openingParen
           yield! nodes pairs
           yield closingParen |]

    member x.Identifier = identifier
    member x.TyparDecls = typarDecls
    member x.OpeningParen = openingParen
    member x.Pairs = pairs
    member x.ClosingParen = closingParen

type PatLongIdentNode
    (
        accessibility: SingleTextNode option,
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        parameters: Pattern list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa accessibility
           yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield! List.map Pattern.Node parameters |]

    member x.Accessibility = accessibility
    member x.Identifier = identifier
    member x.TyparDecls = typarDecls
    member x.Parameters = parameters

type PatParenNode(openingParen: SingleTextNode, pat: Pattern, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingParen; yield Pattern.Node pat; yield closingParen |]

    member x.OpeningParen = openingParen
    member x.Pattern = pat
    member x.ClosingParen = closingParen

type PatTupleNode(pats: Pattern list, range) =
    inherit NodeBase(range)

    override this.Children = [| yield! (List.map Pattern.Node pats) |]
    member x.Patterns = pats

type PatStructTupleNode(pats: Pattern list, range) =
    inherit NodeBase(range)

    override this.Children = [| yield! (List.map Pattern.Node pats) |]
    member x.Patterns = pats

type PatArrayOrListNode(openToken: SingleTextNode, pats: Pattern list, closeToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openToken; yield! List.map Pattern.Node pats; yield closeToken |]

    member x.OpenToken = openToken
    member x.Patterns = pats
    member x.CloseToken = closeToken

type PatRecordField
    (
        prefix: IdentListNode option,
        fieldName: SingleTextNode,
        equals: SingleTextNode,
        pat: Pattern,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa prefix; yield fieldName; yield equals; yield Pattern.Node pat |]

    member x.Prefix = prefix
    member x.FieldName = fieldName
    member x.Equals = equals
    member x.Pattern = pat

type PatRecordNode(openingNode: SingleTextNode, fields: PatRecordField list, closingNode: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield openingNode; yield! nodes fields; yield closingNode |]
    member x.OpeningNode = openingNode
    member x.Fields = fields
    member x.ClosingNode = closingNode

type PatIsInstNode(token: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override this.Children = [| yield token; yield Type.Node t |]
    member x.Token = token
    member x.Type = t

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Pattern =
    | OptionalVal of SingleTextNode
    | Attrib of PatAttribNode
    | Or of PatLeftMiddleRight
    | Ands of PatAndsNode
    | Null of SingleTextNode
    | Wild of SingleTextNode
    | Typed of PatTypedNode
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
        | Attrib n -> n
        | Or n -> n
        | Ands n -> n
        | Null n -> n
        | Wild n -> n
        | Typed n -> n
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

type ExprLazyNode(lazyWord: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield lazyWord; yield Expr.Node expr |]

    member this.LazyWord = lazyWord
    member this.Expr = expr

    member this.ExprIsInfix =
        match Expr.Node expr with
        | :? InfixApp -> true
        | _ -> false

type ExprSingleNode(leading: SingleTextNode, supportsStroustrup: bool, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield leading; yield Expr.Node expr |]

    member _.Leading = leading
    member _.SupportsStroustrup = supportsStroustrup
    member _.Expr = expr

type ExprConstantNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprQuoteNode(openToken: SingleTextNode, expr, closeToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield openToken; yield Expr.Node expr; yield closeToken |]
    member x.OpenToken = openToken
    member x.Expr = expr
    member x.CloseToken = closeToken

type ExprTypedNode(expr: Expr, operator: string, t: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Expr.Node expr; yield Type.Node t |]
    member x.Expr = expr
    member x.Operator = operator
    member x.Type = t

type ExprNewNode(newKeyword: SingleTextNode, t: Type, arguments: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield newKeyword; yield Type.Node t; yield Expr.Node arguments |]

    member x.NewKeyword = newKeyword
    member x.Type = t
    member x.Arguments = arguments

type ExprTupleNode(items: Choice<Expr, SingleTextNode> list, range) =
    inherit NodeBase(range)

    override this.Children =
        items
        |> Seq.map (function
            | Choice1Of2 e -> Expr.Node e
            | Choice2Of2 comma -> comma :> Node)
        |> Seq.toArray

    member x.Items = items

type ExprStructTupleNode(structNode: SingleTextNode, tuple: ExprTupleNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield structNode; yield tuple; yield closingParen |]
    member x.Struct = structNode
    member x.Tuple = tuple
    member x.ClosingParen = closingParen

type ExprArrayOrListNode(openingToken: SingleTextNode, elements: Expr list, closingToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingToken; yield! List.map Expr.Node elements; yield closingToken |]

    member x.Opening = openingToken
    member x.Elements = elements
    member x.Closing = closingToken

type InheritConstructorTypeOnlyNode(inheritKeyword: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield inheritKeyword; yield Type.Node t |]
    member x.InheritKeyword = inheritKeyword
    member x.Type = t

type InheritConstructorUnitNode
    (
        inheritKeyword: SingleTextNode,
        t: Type,
        openingParen: SingleTextNode,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield inheritKeyword
           yield Type.Node t
           yield openingParen
           yield closingParen |]

    member x.InheritKeyword = inheritKeyword
    member x.Type = t
    member x.OpeningParen = openingParen
    member x.ClosingParen = closingParen

type InheritConstructorParenNode(inheritKeyword: SingleTextNode, t: Type, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield inheritKeyword; yield Type.Node t; yield Expr.Node expr |]

    member x.InheritKeyword = inheritKeyword
    member x.Type = t
    member x.Expr = expr

type InheritConstructorOtherNode(inheritKeyword: SingleTextNode, t: Type, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield inheritKeyword; yield Type.Node t; yield Expr.Node expr |]

    member x.InheritKeyword = inheritKeyword
    member x.Type = t
    member x.Expr = expr

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

[<RequireQualifiedAccess; NoComparison>]
type RecordNodeExtra =
    | Inherit of inheritConstructor: InheritConstructor
    | With of expr: Expr
    | None

    static member Node(extra: RecordNodeExtra) : Node option =
        match extra with
        | Inherit n -> Some(InheritConstructor.Node n)
        | With e -> Some(Expr.Node e)
        | None -> Option.None

type RecordFieldNode(fieldName: IdentListNode, equals: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield fieldName; yield equals; yield Expr.Node expr |]
    member x.FieldName = fieldName
    member x.Equals = equals
    member x.Expr = expr

type ExprRecordNode
    (
        openingBrace: SingleTextNode,
        extra: RecordNodeExtra,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingBrace
           yield! noa (RecordNodeExtra.Node extra)
           yield! nodes fields
           yield closingBrace |]

    member x.OpeningBrace = openingBrace
    member x.Extra = extra
    member x.Fields = fields
    member x.ClosingBrace = closingBrace

type AnonRecordFieldNode(ident: SingleTextNode, equals: SingleTextNode, rhs: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield ident; yield equals; yield Expr.Node rhs |]
    member x.Ident = ident
    member x.Equals = equals
    member x.Expr = rhs

type ExprAnonRecordNode
    (
        isStruct: bool,
        openingBrace: SingleTextNode,
        copyInfo: Expr option,
        fields: AnonRecordFieldNode list,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingBrace
           yield! noa (Option.map Expr.Node copyInfo)
           yield! nodes fields
           yield closingBrace |]

    member x.IsStruct = isStruct
    member x.OpeningBrace = openingBrace
    member x.CopyInfo = copyInfo
    member x.Fields = fields
    member x.ClosingBrace = closingBrace

type InterfaceImplNode
    (
        interfaceNode: SingleTextNode,
        t: Type,
        withNode: SingleTextNode option,
        bindings: BindingNode list,
        members: MemberDefn list,
        range
    ) =

    inherit NodeBase(range)

    override this.Children =
        [| yield interfaceNode
           yield Type.Node t
           yield! noa withNode
           yield! nodes bindings
           yield! List.map MemberDefn.Node members |]

    member x.Interface = interfaceNode
    member x.Type = t
    member x.With = withNode
    member x.Bindings = bindings
    member x.Members = members

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
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingBrace
           yield newNode
           yield Type.Node t
           yield! noa (Option.map Expr.Node e)
           yield! noa withNode
           yield! nodes bindings
           yield! List.map MemberDefn.Node members
           yield! nodes interfaces
           yield closingBrace |]

    member x.OpeningBrace = openingBrace
    member x.New = newNode
    member x.Type = t
    member x.Expr = e
    member x.With = withNode
    member x.Bindings = bindings
    member x.Members = members
    member x.Interfaces = interfaces
    member x.ClosingBrace = closingBrace

type ExprWhileNode(whileNode: SingleTextNode, whileExpr: Expr, doExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield whileNode; yield Expr.Node whileExpr; yield Expr.Node doExpr |]

    member x.While = whileNode
    member x.WhileExpr = whileExpr
    member x.DoExpr = doExpr

type ExprForNode
    (
        forNode: SingleTextNode,
        ident: SingleTextNode,
        equals: SingleTextNode,
        identBody: Expr,
        direction: bool,
        toBody: Expr,
        doBody: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield forNode
           yield ident
           yield equals
           yield Expr.Node identBody
           yield Expr.Node toBody
           yield Expr.Node doBody |]

    member x.For = forNode
    member x.Ident = ident
    member x.Equals = equals
    member x.IdentBody = identBody
    member x.Direction = direction
    member x.ToBody = toBody
    member x.DoBody = doBody

type ExprForEachNode(forNode: SingleTextNode, pat: Pattern, enumExpr: Expr, isArrow: bool, bodyExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield forNode
           yield Pattern.Node pat
           yield Expr.Node enumExpr
           yield Expr.Node bodyExpr |]

    member x.For = forNode
    member x.Pattern = pat
    member x.EnumExpr = enumExpr
    member x.IsArrow = isArrow
    member x.BodyExpr = bodyExpr

type ExprNamedComputationNode
    (
        nameExpr: Expr,
        openingBrace: SingleTextNode,
        bodyExpr: Expr,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node nameExpr
           yield openingBrace
           yield Expr.Node bodyExpr
           yield closingBrace |]

    member x.Name = nameExpr
    member x.OpeningBrace = openingBrace
    member x.Body = bodyExpr
    member x.ClosingBrace = closingBrace

type ExprComputationNode(openingBrace: SingleTextNode, bodyExpr: Expr, closingBrace: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingBrace; yield Expr.Node bodyExpr; yield closingBrace |]

    member x.OpeningBrace = openingBrace
    member x.Body = bodyExpr
    member x.ClosingBrace = closingBrace

type ExprLetOrUseNode(binding: BindingNode, inKeyword: SingleTextNode option, range) =
    inherit NodeBase(range)
    override this.Children = [| yield binding; yield! noa inKeyword |]
    member x.Binding = binding
    member x.In = inKeyword

type ExprLetOrUseBangNode(leadingKeyword: SingleTextNode, pat: Pattern, equals: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield leadingKeyword
           yield Pattern.Node pat
           yield equals
           yield Expr.Node expr |]

    member x.LeadingKeyword = leadingKeyword
    member x.Pattern = pat
    member x.Equals = equals
    member x.Expression = expr

type ExprAndBang(leadingKeyword: SingleTextNode, pat: Pattern, equals: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield leadingKeyword
           yield Pattern.Node pat
           yield equals
           yield Expr.Node expr |]

    member x.LeadingKeyword = leadingKeyword
    member x.Pattern = pat
    member x.Equals = equals
    member x.Expression = expr

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

type ExprCompExprBodyNode(statements: ComputationExpressionStatement list, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! List.map ComputationExpressionStatement.Node statements |]

    member x.Statements = statements

type ExprJoinInNode(lhs: Expr, inNode: SingleTextNode, rhs: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Expr.Node lhs; yield inNode; yield Expr.Node rhs |]
    member x.LeftHandSide = lhs
    member x.In = inNode
    member x.RightHandSide = rhs

type ExprParenLambdaNode(openingParen: SingleTextNode, lambda: ExprLambdaNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override this.Children = [| yield openingParen; yield lambda; yield closingParen |]
    member x.OpeningParen = openingParen
    member x.Lambda = lambda
    member x.ClosingParen = closingParen

type ExprLambdaNode(funNode: SingleTextNode, parameters: Pattern list, arrow: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield funNode
           yield! List.map Pattern.Node parameters
           yield arrow
           yield Expr.Node expr |]

    member x.Fun = funNode
    member x.Parameters = parameters
    member x.Arrow = arrow
    member x.Expr = expr

type MatchClauseNode
    (
        bar: SingleTextNode option,
        pattern: Pattern,
        whenExpr: Expr option,
        arrow: SingleTextNode,
        bodyExpr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa bar
           yield Pattern.Node pattern
           yield! noa (Option.map Expr.Node whenExpr)
           yield arrow
           yield Expr.Node bodyExpr |]

    member x.Bar = bar
    member x.Pattern = pattern
    member x.WhenExpr = whenExpr
    member x.Arrow = arrow
    member x.BodyExpr = bodyExpr

type ExprMatchLambdaNode(functionNode: SingleTextNode, clauses: MatchClauseNode list, range) =
    inherit NodeBase(range)

    override this.Children = [| yield functionNode; yield! nodes clauses |]
    member x.Function = functionNode
    member x.Clauses = clauses

type ExprMatchNode
    (
        matchNode: SingleTextNode,
        matchExpr: Expr,
        withNode: SingleTextNode,
        clauses: MatchClauseNode list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield matchNode
           yield Expr.Node matchExpr
           yield withNode
           yield! nodes clauses |]

    member x.Match = matchNode
    member x.MatchExpr = matchExpr
    member x.With = withNode
    member x.Clauses = clauses

type ExprTraitCallNode(t: Type, md: MemberDefn, expr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Type.Node t; yield MemberDefn.Node md; yield Expr.Node expr |]

    member x.Type = t
    member x.MemberDefn = md
    member x.Expr = expr

type ExprParenFunctionNameWithStarNode
    (
        openingParen: SingleTextNode,
        functionName: SingleTextNode,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children = [| yield openingParen; yield functionName; yield closingParen |]
    member x.OpeningParen = openingParen
    member x.FunctionName = functionName
    member x.ClosingParen = closingParen

type ExprParenNode(openingParen: SingleTextNode, expr: Expr, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingParen; yield Expr.Node expr; yield closingParen |]

    member x.OpeningParen = openingParen
    member x.Expr = expr
    member x.ClosingParen = closingParen

type ExprDynamicNode(funcExpr: Expr, argExpr: Expr, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node funcExpr; yield Expr.Node argExpr |]
    member x.FuncExpr = funcExpr
    member x.ArgExpr = argExpr

type ExprPrefixAppNode(operator: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)
    override this.Children = [| yield operator; yield Expr.Node expr |]
    member x.Operator = operator
    member x.Expr = expr

type InfixApp =
    interface
    end

type ExprSameInfixAppsNode(leadingExpr: Expr, subsequentExpressions: (SingleTextNode * Expr) list, range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children =
        let xs =
            List.collect (fun (operator, expr) -> [ (operator :> Node); Expr.Node expr ]) subsequentExpressions

        [| yield Expr.Node leadingExpr; yield! xs |]

    member x.LeadingExpr = leadingExpr
    member x.SubsequentExpressions = subsequentExpressions

type ExprInfixAppNode(lhs: Expr, operator: SingleTextNode, rhs: Expr, range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children = [| yield Expr.Node lhs; yield operator; yield Expr.Node rhs |]
    member x.LeftHandSide = lhs
    member x.RightHandSide: Expr = rhs
    member x.Operator = operator

type ExprIndexWithoutDotNode(identifierExpr: Expr, indexExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Expr.Node identifierExpr; yield Expr.Node indexExpr |]
    member x.Identifier = identifierExpr
    member x.Index = indexExpr

type ExprAppDotGetTypeAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotGetAppDotGetAppParenLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotGetAppParenNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotGetAppWithParenLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotGetAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprAppLongIdentAndSingleParenArgNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprAppSingleParenArgNode(functionExpr: Expr, argExpr: Expr, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node functionExpr; yield Expr.Node argExpr |]
    member x.FunctionExpr = functionExpr
    member x.ArgExpr = argExpr

type ExprDotGetAppWithLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprAppWithLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNestedIndexWithoutDotNode(identifierExpr: Expr, indexExpr: Expr, argumentExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node identifierExpr
           yield Expr.Node indexExpr
           yield Expr.Node argumentExpr |]

    member x.Identifier = identifierExpr
    member x.Index = indexExpr
    member x.Argument = argumentExpr

type ExprEndsWithDualListAppNode
    (
        functionExpr: Expr,
        sequentialExprs: Expr list,
        firstArrayOrList: Expr,
        lastArrayOrList: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node functionExpr
           yield! List.map Expr.Node sequentialExprs
           yield Expr.Node firstArrayOrList
           yield Expr.Node lastArrayOrList |]

    member x.FunctionExpr = functionExpr
    member x.SequentialExpr = sequentialExprs
    member x.FirstArrayOrList = firstArrayOrList
    member x.LastArrayOrList = lastArrayOrList

type ExprEndsWithSingleListAppNode(functionExpr: Expr, sequentialExprs: Expr list, arrayOrList: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node functionExpr
           yield! List.map Expr.Node sequentialExprs
           yield Expr.Node arrayOrList |]

    member x.FunctionExpr = functionExpr
    member x.SequentialExpr = sequentialExprs
    member x.ArrayOrList = arrayOrList

type ExprAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"
    member x.FunctionExpr: Expr = failwith ""
    member x.Arguments: Expr list = failwith ""

type ExprTypeAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTryWithSingleClauseNode
    (
        tryNode: SingleTextNode,
        tryExpr: Expr,
        withNode: SingleTextNode,
        clause: MatchClauseNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield tryNode; yield Expr.Node tryExpr; yield withNode; yield clause |]

    member x.Try = tryNode
    member x.TryExpr = tryExpr
    member x.With = withNode
    member x.Clause = clause

type ExprTryWithNode
    (
        tryNode: SingleTextNode,
        tryExpr: Expr,
        withNode: SingleTextNode,
        clauses: MatchClauseNode list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield tryNode
           yield Expr.Node tryExpr
           yield withNode
           yield! nodes clauses |]

    member x.Try = tryNode
    member x.TryExpr = tryExpr
    member x.With = withNode
    member x.Clauses = clauses

type ExprTryFinallyNode(tryNode: SingleTextNode, tryExpr: Expr, finallyNode: SingleTextNode, finallyExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield tryNode
           yield Expr.Node tryExpr
           yield finallyNode
           yield Expr.Node finallyExpr |]

    member x.Try = tryNode
    member x.TryExpr = tryExpr
    member x.Finally = finallyNode
    member x.FinallyExpr = finallyExpr

type ExprIfThenNode(ifNode: MultipleTextsNode, ifExpr: Expr, thenNode: SingleTextNode, thenExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield ifNode
           yield Expr.Node ifExpr
           yield thenNode
           yield Expr.Node thenExpr |]

    member x.If = ifNode
    member x.IfExpr = ifExpr
    member x.Then = thenNode
    member x.ThenExpr = thenExpr

type ExprIfThenElseNode
    (
        ifNode: MultipleTextsNode,
        ifExpr: Expr,
        thenNode: SingleTextNode,
        thenExpr: Expr,
        elseNode: SingleTextNode,
        elseExpr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield ifNode
           yield Expr.Node ifExpr
           yield thenNode
           yield Expr.Node thenExpr |]

    member x.If = ifNode
    member x.IfExpr = ifExpr
    member x.Then = thenNode
    member x.ThenExpr = thenExpr
    member x.Else = elseNode
    member x.ElseExpr = elseExpr

type ExprIfThenElifNode(branches: ExprIfThenNode list, elseBranch: (SingleTextNode * Expr) option, range) =
    inherit NodeBase(range)

    override this.Children =
        let elseNodes =
            match elseBranch with
            | None -> []
            | Some(elseNode, elseExpr) -> [ yield (elseNode :> Node); yield Expr.Node elseExpr ]

        [| yield! nodes branches; yield! elseNodes |]

    member x.Branches = branches
    member x.Else = elseBranch

type ExprOptVarNode(isOptional: bool, identifier: IdentListNode, range) =
    inherit NodeBase(range)
    override this.Children = [| yield identifier |]
    member x.IsOptional = isOptional
    member x.Identifier = identifier

type ExprLongIdentSetNode(identifier: IdentListNode, rhs: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield identifier; yield Expr.Node rhs |]
    member x.Identifier = identifier
    member x.Expr = rhs

type ExprDotIndexedGetNode(objectExpr: Expr, indexExpr: Expr, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node objectExpr; yield Expr.Node indexExpr |]
    member x.ObjectExpr = objectExpr
    member x.IndexExpr = indexExpr

type ExprDotIndexedSetNode(objectExpr: Expr, indexExpr: Expr, valueExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node objectExpr
           yield Expr.Node indexExpr
           yield Expr.Node valueExpr |]

    member x.ObjectExpr = objectExpr
    member x.Index = indexExpr
    member x.Value = valueExpr

type ExprNamedIndexedPropertySetNode(identifier: IdentListNode, indexExpr: Expr, valueExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield identifier; yield Expr.Node indexExpr; yield Expr.Node valueExpr |]

    member x.Identifier = identifier
    member x.Index = indexExpr
    member x.Value = valueExpr

type ExprDotNamedIndexedPropertySetNode
    (
        identifierExpr: Expr,
        name: IdentListNode,
        propertyExpr: Expr,
        setExpr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node identifierExpr
           yield name
           yield Expr.Node propertyExpr
           yield Expr.Node setExpr |]

    member x.Identifier = identifierExpr
    member x.Name = name
    member x.Property = propertyExpr
    member x.Set = setExpr

type ExprDotGetNode(expr: Expr, identifier: IdentListNode, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node expr; yield identifier |]
    member x.Expr = expr
    member x.Identifier = identifier

type ExprDotSetNode(identifier: Expr, property: IdentListNode, setExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node identifier; yield property; yield Expr.Node setExpr |]

    member x.Identifier = identifier
    member x.Property = property
    member x.Set = setExpr

type ExprSetNode(identifier: Expr, setExpr: Expr, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Expr.Node identifier; yield Expr.Node setExpr |]
    member x.Identifier = identifier
    member x.Set = setExpr

type StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode(typar: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield typar; yield Type.Node t |]
    member x.TypeParameter = typar
    member x.Type = t

type StaticOptimizationConstraint =
    | WhenTyparTyconEqualsTycon of StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode
    | WhenTyparIsStruct of SingleTextNode

    static member Node(c: StaticOptimizationConstraint) : Node =
        match c with
        | WhenTyparTyconEqualsTycon n -> n
        | WhenTyparIsStruct n -> n

type ExprLibraryOnlyStaticOptimizationNode
    (
        optimizedExpr: Expr,
        constraints: StaticOptimizationConstraint list,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield Expr.Node optimizedExpr
           yield! List.map StaticOptimizationConstraint.Node constraints
           yield Expr.Node expr |]

    member x.OptimizedExpr = optimizedExpr
    member x.Constraints = constraints
    member x.Expr = expr

type FillExprNode(expr: Expr, ident: SingleTextNode option, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node expr; yield! noa ident |]
    member x.Expr = expr
    member x.Ident = ident

type ExprInterpolatedStringExprNode(parts: Choice<SingleTextNode, FillExprNode> list, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield!
               List.map
                   (function
                   | Choice1Of2 n -> (n :> Node)
                   | Choice2Of2 n -> (n :> Node))
                   parts |]

    member x.Parts = parts

type ExprIndexRangeWildcardNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprIndexRangeNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprIndexFromEndNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTyparNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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
    | AnonRecord of ExprAnonRecordNode
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
    | AppDotGetTypeApp of ExprAppDotGetTypeAppNode
    | DotGetAppDotGetAppParenLambda of ExprDotGetAppDotGetAppParenLambdaNode
    | DotGetAppParen of ExprDotGetAppParenNode
    | DotGetAppWithParenLambda of ExprDotGetAppWithParenLambdaNode
    | DotGetApp of ExprDotGetAppNode
    | AppLongIdentAndSingleParenArg of ExprAppLongIdentAndSingleParenArgNode
    | AppSingleParenArg of ExprAppSingleParenArgNode
    | DotGetAppWithLambda of ExprDotGetAppWithLambdaNode
    | AppWithLambda of ExprAppWithLambdaNode
    | NestedIndexWithoutDot of ExprNestedIndexWithoutDotNode
    | EndsWithDualListApp of ExprEndsWithDualListAppNode
    | EndsWithSingleListApp of ExprEndsWithSingleListAppNode
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
    | DotGet of ExprDotGetNode
    | DotSet of ExprDotSetNode
    | Set of ExprSetNode
    | LibraryOnlyStaticOptimization of ExprLibraryOnlyStaticOptimizationNode
    | InterpolatedStringExpr of ExprInterpolatedStringExprNode
    | IndexRangeWildcard of ExprIndexRangeWildcardNode
    | IndexRange of ExprIndexRangeNode
    | IndexFromEnd of ExprIndexFromEndNode
    | Typar of ExprTyparNode

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
        | AnonRecord n -> n
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
        | AppDotGetTypeApp n -> n
        | DotGetAppDotGetAppParenLambda n -> n
        | DotGetAppParen n -> n
        | DotGetAppWithParenLambda n -> n
        | DotGetApp n -> n
        | AppLongIdentAndSingleParenArg n -> n
        | AppSingleParenArg n -> n
        | DotGetAppWithLambda n -> n
        | AppWithLambda n -> n
        | NestedIndexWithoutDot n -> n
        | EndsWithDualListApp n -> n
        | EndsWithSingleListApp n -> n
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
        | DotGet n -> n
        | DotSet n -> n
        | Set n -> n
        | LibraryOnlyStaticOptimization n -> n
        | InterpolatedStringExpr n -> n
        | IndexRangeWildcard n -> n
        | IndexRange n -> n
        | IndexFromEnd n -> n
        | Typar n -> n

    member e.IsStroustrupStyleExpr: bool =
        match e with
        | Expr.Record node ->
            match node.Extra with
            | RecordNodeExtra.Inherit _
            | RecordNodeExtra.With _ -> false
            | RecordNodeExtra.None -> true
        // TODO: Exclude records when they have copy info.
        | Expr.AnonRecord _
        | Expr.NamedComputation _
        | Expr.ArrayOrList _ -> true
        | _ -> false

    member e.HasParentheses: bool =
        match e with
        | Expr.Paren _ -> true
        | _ -> false

type OpenModuleOrNamespaceNode(identListNode: IdentListNode, range) =
    inherit NodeBase(range)

    override this.Children = Array.empty
    member x.Name = identListNode

type OpenTargetNode(target: Type, range) =
    inherit NodeBase(range)

    override _.Children = [| yield Type.Node target |]
    member _.Target = target

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

    override this.Children = [| yield! (List.map Open.Node opens) |]
    member x.Opens = opens

type HashDirectiveListNode(hashDirectives: ParsedHashDirectiveNode list) =
    inherit NodeBase(List.map (fun n -> (n :> Node).Range) hashDirectives |> combineRanges)
    override this.Children = [| yield! nodes hashDirectives |]
    member x.HashDirectives = hashDirectives

type AttributeNode(typeName: IdentListNode, expr: Expr option, target: SingleTextNode option, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeName; yield! noa (Option.map Expr.Node expr); yield! noa target |]

    member x.TypeName = typeName
    member x.Expr = expr
    member x.Target = target

/// The content from [< to >]
type AttributeListNode
    (
        openingToken: SingleTextNode,
        attributesNodes: AttributeNode list,
        closingToken: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingToken; yield! nodes attributesNodes; yield closingToken |]

    member x.Opening = openingToken
    member x.Attributes = attributesNodes
    member x.Closing = closingToken

type MultipleAttributeListNode(attributeLists: AttributeListNode list, range) =
    inherit NodeBase(range)
    override this.Children = [| yield! nodes attributeLists |]
    member x.AttributeLists = attributeLists
    member x.IsEmpty = attributeLists.IsEmpty

type ModuleDeclAttributesNode(attributes: MultipleAttributeListNode, doExpr: Expr, range) =
    inherit NodeBase(range)
    override this.Children = [| yield attributes; yield Expr.Node doExpr |]
    member x.Attributes = attributes
    member x.Expr = doExpr

type ExceptionDefnNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        accessibility: SingleTextNode option,
        unionCase: UnionCaseNode,
        withKeyword: SingleTextNode option,
        ms: MemberDefn list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield! noa accessibility
           yield unionCase
           yield! noa withKeyword
           yield! nodes (List.map MemberDefn.Node ms) |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.Accessibility = accessibility
    member x.UnionCase = unionCase
    member x.WithKeyword = withKeyword
    member x.Members = ms

type ExternBindingNode(range) =
    inherit NodeBase(range)
    override this.Children = failwith "todo"

type ModuleAbbrevNode(moduleNode: SingleTextNode, name: SingleTextNode, alias: IdentListNode, range) =
    inherit NodeBase(range)
    override this.Children = [| yield moduleNode; yield name; yield alias |]
    member x.Module = moduleNode
    member x.Name = name
    member x.Alias = alias

type NestedModuleNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        moduleKeyword: SingleTextNode,
        accessibility: SingleTextNode option,
        isRecursive: bool,
        identifier: IdentListNode,
        equalsNode: SingleTextNode,
        decls: ModuleDecl list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield moduleKeyword
           yield! noa accessibility
           yield identifier
           yield equalsNode
           yield! List.map ModuleDecl.Node decls |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.Module = moduleKeyword
    member x.Accessibility = accessibility
    member x.IsRecursive = isRecursive
    member x.Equals = equalsNode
    member x.Decls = decls

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

type BindingReturnInfoNode(colon: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override x.Children = [| yield colon; yield Type.Node t |]
    member x.Colon = colon
    member x.Type = t

type BindingNode
    (
        leadingKeyword: MultipleTextsNode,
        functionName: Choice<SingleTextNode, Pattern>,
        parameters: Pattern list,
        returnType: BindingReturnInfoNode option,
        equals: SingleTextNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.FunctionName = functionName
    member x.Parameters = parameters
    member x.ReturnType = returnType
    member x.Equals = equals
    member x.Expr = expr

    override this.Children =
        [| yield leadingKeyword
           yield
               match functionName with
               | Choice1Of2 n -> (n :> Node)
               | Choice2Of2 p -> Pattern.Node p
           yield! nodes (List.map Pattern.Node parameters)
           yield! noa returnType
           yield equals
           yield Expr.Node expr |]

type BindingListNode(bindings: BindingNode list, range) =
    inherit NodeBase(range)
    override x.Children = [| yield! nodes bindings |]
    member x.Bindings = bindings

type FieldNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        leadingKeyword: MultipleTextsNode option,
        isMutable: bool,
        accessibility: SingleTextNode option,
        name: SingleTextNode option,
        t: Type,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield! noa leadingKeyword
           yield! noa accessibility
           yield! noa name
           yield Type.Node t |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.LeadingKeyword = leadingKeyword
    member x.IsMutable = isMutable
    member x.Accessibility = accessibility
    member x.Name = name
    member x.Type = t

type UnionCaseNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        bar: SingleTextNode option,
        identifier: SingleTextNode,
        fields: FieldNode list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield! noa bar
           yield identifier
           yield! nodes fields |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.Bar = bar
    member x.Identifier = identifier
    member x.Fields = fields

type TypeNameNode
    (
        xmlDoc: SingleTextNode option,
        attrs: MultipleAttributeListNode,
        leadingKeyword: SingleTextNode,
        ao: SingleTextNode option,
        identifier: IdentListNode,
        typeParams: TyparDecls option,
        constraints: TypeConstraint list,
        equalsToken: SingleTextNode option,
        withKeyword: SingleTextNode option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attrs
           yield leadingKeyword
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield! List.map TypeConstraint.Node constraints
           yield! noa ao
           yield identifier
           yield! noa equalsToken
           yield! noa withKeyword |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attrs
    member x.IsFirstType = leadingKeyword.Text = "type"
    member x.LeadingKeyword = leadingKeyword
    member x.Accessibility = ao
    member x.Identifier = identifier
    member x.TypeParameters = typeParams
    member x.Constraints = constraints
    member x.EqualsToken = equalsToken
    member x.WithKeyword = withKeyword

type ITypeDefn =
    abstract member TypeName: TypeNameNode
    abstract member Members: MemberDefn list

type EnumCaseNode
    (
        xmlDoc: SingleTextNode option,
        bar: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        identifier: SingleTextNode,
        equals: SingleTextNode,
        constant: Constant,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield! noa bar
           yield identifier
           yield equals
           yield Constant.Node constant |]

    member x.XmlDoc = xmlDoc
    member x.Bar = bar
    member x.Attributes = attributes
    member x.Identifier = identifier
    member x.Equals = equals
    member x.Constant = constant

type TypeDefnEnumNode(typeNameNode, enumCases: EnumCaseNode list, members: MemberDefn list, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeNameNode
           yield! nodes enumCases
           yield! nodes (List.map MemberDefn.Node members) |]

    member x.EnumCases = enumCases

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = members

type TypeDefnUnionNode
    (
        typeNameNode,
        accessibility: SingleTextNode option,
        unionCases: UnionCaseNode list,
        members: MemberDefn list,
        range
    )

 =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeNameNode
           yield! noa accessibility
           yield! nodes unionCases
           yield! nodes (List.map MemberDefn.Node members) |]

    member x.Accessibility = accessibility
    member x.UnionCases = unionCases

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = members

type TypeDefnRecordNode
    (
        typeNameNode,
        accessibility: SingleTextNode option,
        openingBrace: SingleTextNode,
        fields: FieldNode list,
        closingBrace: SingleTextNode,
        members,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeNameNode
           yield! noa accessibility
           yield openingBrace
           yield! nodes fields
           yield closingBrace
           yield! nodes (List.map MemberDefn.Node members) |]

    member x.Accessibility = accessibility
    member x.OpeningBrace = openingBrace
    member x.Fields = fields
    member x.ClosingBrace = closingBrace

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = members

type TypeDefnAbbrevNode(typeNameNode, t: Type, range) =
    inherit NodeBase(range)

    override _.Children = [| yield typeNameNode; yield Type.Node t |]
    member _.Type = t

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = []

type SimplePatNode
    (
        attributes: MultipleAttributeListNode,
        isOptional: bool,
        identifier: SingleTextNode,
        t: Type option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield attributes; yield identifier; yield! noa (Option.map Type.Node t) |]

    member x.Attributes = attributes
    member x.IsOptional = isOptional
    member x.Identifier = identifier
    member x.Type = t

type ImplicitConstructorNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        accessibility: SingleTextNode option,
        openingParen: SingleTextNode,
        parameters: SimplePatNode list,
        closingParen: SingleTextNode,
        self: SingleTextNode option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield! noa accessibility
           yield openingParen
           yield! nodes parameters
           yield closingParen
           yield! noa self |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.Accessibility = accessibility
    member x.OpeningParen = openingParen
    member x.Parameters = parameters
    member x.ClosingParen = closingParen
    member x.Self = self

type TypeDefnExplicitBodyNode(kind: SingleTextNode, members: MemberDefn list, endNode: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield kind; yield! nodes (List.map MemberDefn.Node members); yield endNode |]

    member x.Kind = kind
    member x.Members = members
    member x.End = endNode

type TypeDefnExplicitNode
    (
        typeNameNode,
        implicitCtor: ImplicitConstructorNode option,
        body: TypeDefnExplicitBodyNode,
        members,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeNameNode
           yield! noa implicitCtor
           yield body
           yield! nodes (List.map MemberDefn.Node members) |]

    member x.ImplicitConstructor = implicitCtor
    member x.Body = body

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = members

type TypeDefnAugmentationNode(typeNameNode, members, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeNameNode; yield! (List.map MemberDefn.Node members) |]

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = members

type TypeDefnDelegateNode(typeNameNode, delegateNode: SingleTextNode, typeList: TypeFunsNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield typeNameNode; yield delegateNode; yield typeList |]

    member x.DelegateNode = delegateNode
    member x.TypeList = typeList

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = List.empty

type TypeDefnRegularNode(typeNameNode, implicitCtor: ImplicitConstructorNode option, members, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeNameNode; yield! List.map MemberDefn.Node members |]

    member x.ImplicitConstructor = implicitCtor

    interface ITypeDefn with
        member x.TypeName = typeNameNode
        member x.Members = members

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
                member x.TypeName = n
                member x.Members = [] }
        | Abbrev n -> n
        | Explicit n -> n
        | Augmentation n -> n
        | Delegate n -> n
        | Regular n -> n

type MemberDefnInheritNode(inheritKeyword: SingleTextNode, baseType: Type, range) =
    inherit NodeBase(range)

    override this.Children = [| yield inheritKeyword; yield Type.Node baseType |]

    member this.Inherit = inheritKeyword
    member this.BaseType = baseType

type MemberDefnExternBindingNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnExplicitCtorNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        accessibility: SingleTextNode option,
        newKeyword: SingleTextNode,
        pat: Pattern,
        alias: SingleTextNode option,
        equals: SingleTextNode,
        expr: Expr,
        thenExpr: Expr option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield! noa accessibility
           yield newKeyword
           yield Pattern.Node pat
           yield! noa alias
           yield equals
           yield Expr.Node expr
           yield! noa (Option.map Expr.Node thenExpr) |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.Accessibility = accessibility
    member x.New = newKeyword
    member x.Pattern = pat
    member x.Alias = alias
    member x.Equals = equals
    member x.Expr = expr
    member x.ThenExpr = thenExpr

type MemberDefnInterfaceNode
    (
        interfaceNode: SingleTextNode,
        t: Type,
        withNode: SingleTextNode option,
        members: MemberDefn list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield interfaceNode
           yield Type.Node t
           yield! noa withNode
           yield! List.map MemberDefn.Node members |]

    member x.Interface = interfaceNode
    member x.Type = t
    member x.With = withNode
    member x.Members = members

type MemberDefnAutoPropertyNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        leadingKeyword: MultipleTextsNode,
        accessibility: SingleTextNode option,
        identifier: SingleTextNode,
        t: Type option,
        equals: SingleTextNode,
        expr: Expr,
        withGetSet: MultipleTextsNode option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield leadingKeyword
           yield! noa accessibility
           yield identifier
           yield! noa (Option.map Type.Node t)
           yield equals
           yield Expr.Node expr
           yield! noa withGetSet |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.LeadingKeyword = leadingKeyword
    member x.Accessibility = accessibility
    member x.Identifier = identifier
    member x.Type = t
    member x.Equals = equals
    member x.Expr = expr
    member x.WithGetSet = withGetSet

type MemberDefnAbstractSlotNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        leadingKeyword: MultipleTextsNode,
        identifier: SingleTextNode,
        typeParams: TyparDecls option,
        t: Type,
        withGetSet: MultipleTextsNode option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield leadingKeyword
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield Type.Node t
           yield! noa withGetSet |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.LeadingKeyword = leadingKeyword
    member x.Identifier = identifier
    member x.TypeParams = typeParams
    member x.Type = t
    member x.WithGetSet = withGetSet

type PropertyGetSetBindingNode
    (
        accessibility: SingleTextNode option,
        leadingKeyword: SingleTextNode,
        parameters: Pattern list,
        returnType: BindingReturnInfoNode option,
        equals: SingleTextNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa accessibility
           yield leadingKeyword
           yield! List.map Pattern.Node parameters
           yield! noa returnType
           yield equals
           yield Expr.Node expr |]

    member x.Accessibility = accessibility
    member x.LeadingKeyword = leadingKeyword
    member x.Parameters = parameters
    member x.ReturnType = returnType
    member x.Equals = equals
    member x.Expr = expr

type MemberDefnPropertyGetSetNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        leadingKeyword: MultipleTextsNode,
        isInline: bool,
        accessibility: SingleTextNode option,
        memberName: IdentListNode,
        withKeyword: SingleTextNode,
        firstBinding: PropertyGetSetBindingNode,
        andKeyword: SingleTextNode option,
        lastBinding: PropertyGetSetBindingNode option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield leadingKeyword
           yield! noa accessibility
           yield memberName
           yield withKeyword
           yield firstBinding
           yield! noa andKeyword
           yield! noa lastBinding |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.LeadingKeyword = leadingKeyword
    member x.IsInline = isInline
    member x.Accessibility = accessibility
    member x.MemberName = memberName
    member x.WithKeyword = withKeyword
    member x.FirstBinding = firstBinding
    member x.AndKeyword = andKeyword
    member x.LastBinding = lastBinding

type ValNode
    (
        xmlDoc: SingleTextNode option,
        attributes: MultipleAttributeListNode,
        leadingKeyword: MultipleTextsNode option,
        isInline: bool,
        isMutable: bool,
        accessibility: SingleTextNode option,
        identifier: SingleTextNode,
        typeParams: TyparDecls option,
        t: Type,
        equals: SingleTextNode option,
        eo: Expr option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa xmlDoc
           yield attributes
           yield! noa leadingKeyword
           yield! noa accessibility
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield Type.Node t
           yield! noa equals
           yield! noa (Option.map Expr.Node eo) |]

    member x.XmlDoc = xmlDoc
    member x.Attributes = attributes
    member x.LeadingKeyword = leadingKeyword
    member x.IsInline = isInline
    member x.IsMutable = isMutable
    member x.Accessibility = accessibility
    member x.Identifier = identifier
    member x.TypeParams = typeParams
    member x.Type = t
    member x.Equals = equals
    member x.Expr = eo

type MemberDefnSigMemberNode(valNode: ValNode, withGetSet: MultipleTextsNode option, range) =
    inherit NodeBase(range)
    override this.Children = [| yield valNode; yield! noa withGetSet |]
    member x.Val = valNode
    member x.WithGetSet = withGetSet

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MemberDefn =
    | ImplicitInherit of InheritConstructor
    | Inherit of MemberDefnInheritNode
    | ValField of FieldNode
    | Member of BindingNode
    | ExternBinding of MemberDefnExternBindingNode
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

type UnitNode(openingParen: SingleTextNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override x.Children = [| yield openingParen; yield closingParen |]
    member x.OpeningParen = openingParen
    member x.ClosingParen = closingParen

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Constant =
    | FromText of SingleTextNode
    | Unit of UnitNode

    static member Node(c: Constant) : NodeBase =
        match c with
        | FromText n -> n
        | Unit n -> n

type TyparDeclsPostfixListNode(range) =
    inherit NodeBase(range)
    override this.Children = failwith "todo"

type TyparDeclsPrefixListNode(range) =
    inherit NodeBase(range)
    override this.Children = failwith "todo"

type TyparDeclsSinglePrefixNode(range) =
    inherit NodeBase(range)
    override this.Children = failwith "todo"

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TyparDecls =
    | PostfixList of TyparDeclsPostfixListNode
    | PrefixList of TyparDeclsPrefixListNode
    | SinglePrefix of TyparDeclsSinglePrefixNode

    static member Node(td: TyparDecls) : Node =
        match td with
        | PostfixList n -> n
        | PrefixList n -> n
        | SinglePrefix n -> n

type TypeConstraintSingleNode(typar: SingleTextNode, kind: SingleTextNode, range) =
    inherit NodeBase(range)
    override this.Children = [| yield typar; yield kind |]
    member x.Typar = typar
    member x.Kind = kind

type TypeConstraintDefaultsToTypeNode(defaultNode: SingleTextNode, typar: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override this.Children = [| yield defaultNode; yield typar; yield Type.Node t |]
    member x.Default = defaultNode
    member x.Typar = typar
    member x.Type = t

type TypeConstraintSubtypeOfTypeNode(typar: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override this.Children = [| yield typar; yield Type.Node t |]
    member x.Typar = typar
    member x.Type = t

// TODO: MemberSig
type TypeConstraintSupportsMemberNode(t: Type, memberSig: obj, range) =
    inherit NodeBase(range)
    override this.Children = [| yield Type.Node t |]
    member x.Type = t
    member x.MemberSig = memberSig

type TypeConstraintEnumOrDelegateNode(typar: SingleTextNode, verb: string, ts: Type list, range) =
    inherit NodeBase(range)
    override this.Children = [| yield typar; yield! List.map Type.Node ts |]
    member x.Typar = typar
    member x.Verb = verb
    member x.Types = ts

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
