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

type Oak(parsedHashDirectives: ParsedHashDirectiveNode list, modulesOrNamespaces: ModuleOrNamespaceNode list) =
    inherit NodeBase([| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]
                     |> Seq.map nodeRange
                     |> combineRanges)

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

type TypeFunsNode(ts: (Type * SingleTextNode) list, returnType: Type, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! nodes (List.collect (fun (t, arrow) -> [ yield Type.Node t; yield (arrow :> Node) ]) ts)
           yield Type.Node returnType |]

    member x.Parameters = ts
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

type TypeStaticConstantExprNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeStaticConstantNamedNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeArrayNode(t: Type, rank: int, range) =
    inherit NodeBase(range)

    override this.Children = [| yield Type.Node t |]
    member x.Type = t
    member x.Rank = rank

type TypeAnonNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeVarNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeLongIdentAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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

type TypeWithGlobalConstraintsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeAnonRecordNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeParenNode(openingParen: SingleTextNode, t: Type, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield openingParen; yield Type.Node t; yield closingParen |]

    member x.OpeningParen = openingParen
    member x.Type = t
    member x.ClosingParen = closingParen

type TypeSignatureParameterNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeOrNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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
    | Anon of TypeAnonNode
    | Var of TypeVarNode
    | App of TypeAppNode
    | LongIdentApp of TypeLongIdentAppNode
    | StructTuple of TypeStructTupleNode
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
        | App n -> n
        | LongIdentApp n -> n
        | StructTuple n -> n
        | WithGlobalConstraints n -> n
        | LongIdent n -> n
        | AnonRecord n -> n
        | Paren n -> n
        | SignatureParameter n -> n
        | Or n -> n

type PatAttribNode(attrs: AttributesListNode, pat: Pattern, range) =
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

type PatNamedNode(name: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield name |]
    member this.Name = name

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
        ao: SingleTextNode option,
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        parameters: Pattern list,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield! noa ao
           yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield! List.map Pattern.Node parameters |]

    member x.Accessibility = ao
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

type ExprNewParenNode(newKeyword: SingleTextNode, t: Type, arguments: Expr, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield newKeyword; yield Type.Node t; yield Expr.Node arguments |]

    member x.NewKeyword = newKeyword
    member x.Type = t
    member x.Arguments = arguments

type ExprNewNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTupleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprStructTupleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprArrayOrListNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprRecordNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprAnonRecordNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprObjExprNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprWhileNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprForNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprForEachNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNamedComputationNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprComputationNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprCompExprBodyNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprJoinInNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprParenLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprMatchLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprMatchNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTraitCallNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprParenILEmbeddedNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprParenFunctionNameWithStarNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprParenNode(openingParen: SingleTextNode, expr: Expr, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield openingParen; yield Expr.Node expr; yield closingParen |]

    member x.OpeningParen = openingParen
    member x.Expr = expr
    member x.ClosingParen = closingParen

type ExprDynamicNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprPrefixAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type InfixApp =
    interface
    end

type ExprNewlineInfixAppAlwaysMultilineNode(range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children = failwith "todo"

type ExprNewlineInfixAppsNode(range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children = failwith "todo"

type ExprSameInfixAppsNode(range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children = failwith "todo"

type ExprInfixAppNode(range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children = failwith "todo"

type ExprTernaryAppNode(range) =
    inherit NodeBase(range)
    interface InfixApp

    override this.Children = failwith "todo"

type ExprIndexWithoutDotNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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

type ExprAppSingleParenArgNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotGetAppWithLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprAppWithLambdaNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNestedIndexWithoutDotNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprEndsWithDualListAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprEndsWithSingleListAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTypeAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprLetOrUsesNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTryWithSingleClauseNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTryWithNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTryFinallyNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprSequentialsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprIfThenNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprIfThenElseNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprIfThenElifNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprOptVarNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprLongIdentSetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotIndexedGetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotIndexedSetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNamedIndexedPropertySetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotNamedIndexedPropertySetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotGetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDotSetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprSetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprLibraryOnlyStaticOptimizationNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprInterpolatedStringExprNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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
    | NewParen of ExprNewParenNode
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
    | ParenILEmbedded of ExprParenILEmbeddedNode
    | ParenFunctionNameWithStar of ExprParenFunctionNameWithStarNode
    | Paren of ExprParenNode
    | Dynamic of ExprDynamicNode
    | PrefixApp of ExprPrefixAppNode
    | NewlineInfixAppAlwaysMultiline of ExprNewlineInfixAppAlwaysMultilineNode
    | NewlineInfixApps of ExprNewlineInfixAppsNode
    | SameInfixApps of ExprSameInfixAppsNode
    | InfixApp of ExprInfixAppNode
    | TernaryApp of ExprTernaryAppNode
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
    | LetOrUses of ExprLetOrUsesNode
    | TryWithSingleClause of ExprTryWithSingleClauseNode
    | TryWith of ExprTryWithNode
    | TryFinally of ExprTryFinallyNode
    | Sequentials of ExprSequentialsNode
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
        | NewParen n -> n
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
        | NewlineInfixAppAlwaysMultiline n -> n
        | NewlineInfixApps n -> n
        | SameInfixApps n -> n
        | InfixApp n -> n
        | TernaryApp n -> n
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
        | LetOrUses n -> n
        | TryWithSingleClause n -> n
        | TryWith n -> n
        | TryFinally n -> n
        | Sequentials n -> n
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
        // TODO: Exclude records when they have copy info.
        | Expr.AnonRecord _
        | Expr.Record _
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

type HashDirectiveListNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type AttributeNode(typeName: IdentListNode, expr: Expr option, target: SingleTextNode option, range) =
    inherit NodeBase(range)

    override this.Children =
        [| yield typeName; yield! noa (Option.map Expr.Node expr); yield! noa target |]

    member x.TypeName = typeName
    member x.Expr = expr
    member x.Target = target

type AttributesNode(attributes: AttributeNode list, range) =
    inherit NodeBase(range)
    override this.Children = [| yield! nodes attributes |]
    member x.Attributes = attributes

type AttributesListNode(attributesNodes: AttributesNode list, range) =
    inherit NodeBase(range)
    override this.Children = [| yield! nodes attributesNodes |]
    static member Empty = AttributesListNode([], Range.Zero)

    member x.AllAttributes =
        List.collect (fun (an: AttributesNode) -> an.Attributes) attributesNodes

type ExternBindingNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ModuleAbbrevNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

/// Each case in this DU should have a container node
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ModuleDecl =
    | OpenList of OpenListNode
    | HashDirectiveList of HashDirectiveListNode
    | AttributesList of AttributesListNode
    | DeclExpr of Expr
    | ExternBinding of ExternBindingNode
    | TopLevelBinding of BindingNode
    | ModuleAbbrev of ModuleAbbrevNode
    | NestedModule of ModuleOrNamespaceNode
    | TypeDefn of TypeDefn

    static member Node(x: ModuleDecl) : Node =
        match x with
        | OpenList n -> n
        | HashDirectiveList n -> n
        | AttributesList n -> n
        | DeclExpr e -> Expr.Node e
        | ExternBinding n -> n
        | TopLevelBinding n -> n
        | ModuleAbbrev n -> n
        | NestedModule n -> n
        | TypeDefn t -> TypeDefn.Node t

type BindingNode
    (
        leadingKeyword: SingleTextNode,
        functionName: Choice<SingleTextNode, Pattern>,
        parameters: Pattern list,
        returnTypeNodes: (SingleTextNode * Type) option,
        equals: SingleTextNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)
    member x.LeadingKeyword = leadingKeyword
    member x.FunctionName = functionName
    member x.Parameters = parameters
    member x.ReturnType = returnTypeNodes
    member x.Equals = equals
    member x.Expr = expr

    override this.Children =
        [| yield leadingKeyword
           yield
               match functionName with
               | Choice1Of2 n -> (n :> Node)
               | Choice2Of2 p -> Pattern.Node p
           yield! nodes (List.map Pattern.Node parameters)
           yield!
               match returnTypeNodes with
               | None -> Array.empty
               | Some (colon, t) -> [| colon :> Node; Type.Node t |]
           yield equals
           yield Expr.Node expr |]

type TypeNameNode
    (
        attrs: AttributesListNode,
        leadingKeyword: SingleTextNode,
        isFirstType: bool,
        ao: SingleTextNode option,
        identifier: IdentListNode,
        typeParams: obj option,
        equalsToken: SingleTextNode option,
        withKeyword: SingleTextNode option,
        range
    ) =
    inherit NodeBase(range)

    override this.Children =
        [| yield attrs
           yield leadingKeyword
           yield! noa ao
           yield identifier
           yield! noa equalsToken
           yield! noa withKeyword |]

    member x.IsFirstType = isFirstType
    member x.LeadingKeyword = leadingKeyword
    member x.Identifier = identifier
    member x.EqualsToken = equalsToken

type ITypeDefn =
    abstract member TypeName: TypeNameNode

type TypeDefnEnumNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnUnionNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnRecordNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnNoneNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnAbbrevNode(typeNameNode, t: Type, range) =
    inherit NodeBase(range)

    override _.Children = [| yield typeNameNode; yield Type.Node t |]
    member _.Type = t

    interface ITypeDefn with
        member x.TypeName = typeNameNode

type TypeDefnExceptionNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnExplicitClassOrInterfaceOrStructNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnAugmentationNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnFunNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnDelegateNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnUnspecifiedNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

type TypeDefnRegularTypeNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

    interface ITypeDefn with
        member x.TypeName = failwith "todo"

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TypeDefn =
    | Enum of TypeDefnEnumNode
    | Union of TypeDefnUnionNode
    | Record of TypeDefnRecordNode
    | None of TypeDefnNoneNode
    | Abbrev of TypeDefnAbbrevNode
    | Exception of TypeDefnExceptionNode
    | ExplicitClassOrInterfaceOrStruct of TypeDefnExplicitClassOrInterfaceOrStructNode
    | Augmentation of TypeDefnAugmentationNode
    | Fun of TypeDefnFunNode
    | Delegate of TypeDefnDelegateNode
    | Unspecified of TypeDefnUnspecifiedNode
    | RegularType of TypeDefnRegularTypeNode

    static member Node(x: TypeDefn) : Node =
        match x with
        | Enum n -> n
        | Union n -> n
        | Record n -> n
        | None n -> n
        | Abbrev n -> n
        | Exception n -> n
        | ExplicitClassOrInterfaceOrStruct n -> n
        | Augmentation n -> n
        | Fun n -> n
        | Delegate n -> n
        | Unspecified n -> n
        | RegularType n -> n

    static member TypeDefnNode(x: TypeDefn) : ITypeDefn =
        match x with
        | Enum n -> n
        | Union n -> n
        | Record n -> n
        | None n -> n
        | Abbrev n -> n
        | Exception n -> n
        | ExplicitClassOrInterfaceOrStruct n -> n
        | Augmentation n -> n
        | Fun n -> n
        | Delegate n -> n
        | Unspecified n -> n
        | RegularType n -> n

type MemberDefnImplicitInheritNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnInheritNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnValFieldNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnImplicitCtorNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnMemberNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnExternBindingNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnLetBindingNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnExplicitCtorNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnInterfaceNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnAutoPropertyNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnAbstractSlotNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type MemberDefnPropertyGetSetNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MemberDefn =
    | Open of OpenListNode
    | ImplicitInherit of MemberDefnImplicitInheritNode
    | Inherit of MemberDefnInheritNode
    | ValField of MemberDefnValFieldNode
    | ImplicitCtor of MemberDefnImplicitCtorNode
    | Member of MemberDefnMemberNode
    | ExternBinding of MemberDefnExternBindingNode
    | LetBinding of MemberDefnLetBindingNode
    | ExplicitCtor of MemberDefnExplicitCtorNode
    | Interface of MemberDefnInterfaceNode
    | AutoProperty of MemberDefnAutoPropertyNode
    | AbstractSlot of MemberDefnAbstractSlotNode
    | PropertyGetSet of MemberDefnPropertyGetSetNode

    static member Node(md: MemberDefn) : NodeBase =
        match md with
        | Open n -> n
        | ImplicitInherit n -> n
        | Inherit n -> n
        | ValField n -> n
        | ImplicitCtor n -> n
        | Member n -> n
        | ExternBinding n -> n
        | LetBinding n -> n
        | ExplicitCtor n -> n
        | Interface n -> n
        | AutoProperty n -> n
        | AbstractSlot n -> n
        | PropertyGetSet n -> n

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
