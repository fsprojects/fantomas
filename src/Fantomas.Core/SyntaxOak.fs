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
    | Ident of SingleTextNode
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
           yield name
           yield! List.map ModuleDecl.Node decls |]

type TypeFunsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeTupleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeHashConstraintNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeMeasurePowerNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeMeasureDivideNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeStaticConstantNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeStaticConstantExprNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeStaticConstantNamedNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeArrayNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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

type TypeStructTupleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeWithGlobalConstraintsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeAnonRecordNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type TypeParenNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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
    | MeasureDivide of TypeMeasureDivideNode
    | StaticConstant of TypeStaticConstantNode
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

    static member Node(x: Type) : NodeBase =
        match x with
        | Funs n -> n
        | Tuple n -> n
        | HashConstraint n -> n
        | MeasurePower n -> n
        | MeasureDivide n -> n
        | StaticConstant n -> n
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

type PatOptionalValNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatAttribNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatOrNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatAndsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatNullNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatWildNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatTypedNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatNamedNode(name: SingleTextNode, range) =
    inherit NodeBase(range)

    override this.Children = [| yield name |]
    member this.Name = name

type PatAsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatListConsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatNamePatPairsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatLongIdentParenNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatLongIdentNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatUnitNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatParenNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatTupleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatStructTupleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatArrayOrListNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatRecordNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatConstNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatIsInstNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type PatQuoteExprNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Pattern =
    | OptionalVal of PatOptionalValNode
    | Attrib of PatAttribNode
    | Or of PatOrNode
    | Ands of PatAndsNode
    | Null of PatNullNode
    | Wild of PatWildNode
    | Typed of PatTypedNode
    | Named of PatNamedNode
    | As of PatAsNode
    | ListCons of PatListConsNode
    | NamePatPairs of PatNamePatPairsNode
    | LongIdentParen of PatLongIdentParenNode
    | LongIdent of PatLongIdentNode
    | Unit of PatUnitNode
    | Paren of PatParenNode
    | Tuple of PatTupleNode
    | StructTuple of PatStructTupleNode
    | ArrayOrList of PatArrayOrListNode
    | Record of PatRecordNode
    | Const of PatConstNode
    | IsInst of PatIsInstNode
    | QuoteExpr of PatQuoteExprNode

    static member Node(x: Pattern) : NodeBase =
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
        | LongIdentParen n -> n
        | LongIdent n -> n
        | Unit n -> n
        | Paren n -> n
        | Tuple n -> n
        | StructTuple n -> n
        | ArrayOrList n -> n
        | Record n -> n
        | Const n -> n
        | IsInst n -> n
        | QuoteExpr n -> n

type ExprLazyNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprSingleNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprConstantNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNullNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprQuoteNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTypedNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

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

type ExprParenNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprDynamicNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprPrefixAppNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNewlineInfixAppAlwaysMultilineNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprNewlineInfixAppsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprSameInfixAppsNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type ExprTernaryAppNode(range) =
    inherit NodeBase(range)

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

type ExprIdentNode(range) =
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
    | Constant of SingleTextNode
    | Null of ExprNullNode
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
    | ParenILEmbedded of ExprParenILEmbeddedNode
    | ParenFunctionNameWithStar of ExprParenFunctionNameWithStarNode
    | Paren of ExprParenNode
    | Dynamic of ExprDynamicNode
    | PrefixApp of ExprPrefixAppNode
    | NewlineInfixAppAlwaysMultiline of ExprNewlineInfixAppAlwaysMultilineNode
    | NewlineInfixApps of ExprNewlineInfixAppsNode
    | SameInfixApps of ExprSameInfixAppsNode
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
    | Ident of ExprIdentNode
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

    static member Node(x: Expr) : NodeBase =
        match x with
        | Lazy n -> n
        | Single n -> n
        | Constant n -> n
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
        | NewlineInfixAppAlwaysMultiline n -> n
        | NewlineInfixApps n -> n
        | SameInfixApps n -> n
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

    static member Node(x: Open) : NodeBase =
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

type AttributesNode(range) =
    inherit NodeBase(range)

    override this.Children = failwith "todo"

type AttributesListNode(attributesNodes: AttributesNode list, range) =
    inherit NodeBase(range)

    override this.Children = [| yield! nodes attributesNodes |]
    static member Empty = AttributesListNode([], Range.Zero)

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

    static member Node(x: ModuleDecl) : NodeBase =
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
        functionName: SingleTextNode,
        parameters: Pattern seq,
        equals: SingleTextNode,
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
        [| yield leadingKeyword
           yield functionName
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

    static member Node(x: TypeDefn) : NodeBase =
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
