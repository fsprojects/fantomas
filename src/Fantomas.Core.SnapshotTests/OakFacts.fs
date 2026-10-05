/// What a case's Oak contains: its nodes, the trivia attached to them, and the shape of each node.
/// The checks and the reports read the tree through this and nothing else.
module Fantomas.Core.SnapshotTests.OakFacts

open System
open System.Collections
open System.Reflection
open Microsoft.FSharp.Reflection
open Fantomas.Core.SyntaxOak

/// A node and the node whose `Children` it was found in. The root has no parent.
[<NoComparison; NoEquality>]
type Visit = { Node: Node; Parent: Node option }

/// Every node of the tree, parents before their children, reached through `Children`.
let visits (oak: Oak) : Visit list =
    let rec walk (parent: Node option) (node: Node) : Visit list =
        { Node = node; Parent = parent }
        :: (node.Children |> Array.toList |> List.collect (walk (Some node)))

    walk None oak

/// What a piece of trivia is, as the reports name it. `None` for a cursor, which is not trivia a
/// person wrote.
let triviaKind (content: TriviaContent) : string option =
    match content with
    | TriviaContent.Cursor -> None
    | TriviaContent.Newline -> Some "blank line"
    | TriviaContent.Directive _ -> Some "directive"
    | TriviaContent.BlockComment _ -> Some "block comment"
    | TriviaContent.LineCommentAfterSourceCode _ -> Some "line comment"
    | TriviaContent.CommentOnSingleLine _
    | TriviaContent.CommentOnSingleLineWithLeadingNewlines _ -> Some "own-line comment"

/// One piece of trivia where it ended up: on which node, before or after it, and of what kind.
[<NoComparison; NoEquality>]
type Attachment =
    {
        Visit: Visit
        IsBefore: bool
        Kind: string
        /// Where the trivia is in the source.
        Range: Fantomas.FCS.Text.range
    }

let attachments (oak: Oak) : Attachment list =
    visits oak
    |> List.collect (fun (visit: Visit) ->
        let side (isBefore: bool) (trivia: TriviaNode seq) : Attachment list =
            trivia
            |> Seq.choose (fun (trivia: TriviaNode) ->
                triviaKind trivia.Content
                |> Option.map (fun (kind: string) ->
                    {
                        Visit = visit
                        IsBefore = isBefore
                        Kind = kind
                        Range = trivia.Range
                    }
                )
            )
            |> Seq.toList

        side true visit.Node.ContentBefore @ side false visit.Node.ContentAfter
    )

/// The type holding every Oak node and union, `Fantomas.Core.SyntaxOak`.
let syntaxOakModule: System.Type = typeof<Oak>.DeclaringType

/// Every type `Fantomas.Core.SyntaxOak` declares: the node classes, the unions and the interfaces.
let syntaxOakTypes: System.Type array =
    syntaxOakModule.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)

/// Every concrete node class of the Oak.
let nodeClasses: System.Type list =
    syntaxOakTypes
    |> Array.filter (fun (t: System.Type) -> typeof<Node>.IsAssignableFrom t && not t.IsAbstract && not t.IsInterface)
    |> Array.sortBy (fun (t: System.Type) -> t.Name)
    |> Array.toList

let private isGeneric (definition: System.Type) (t: System.Type) : bool =
    t.IsGenericType && t.GetGenericTypeDefinition() = definition

let isOption (t: System.Type) : bool = isGeneric typedefof<obj option> t

let isList (t: System.Type) : bool = isGeneric typedefof<obj list> t

/// The properties a node class has itself, leaving out what every node has through `Node` and
/// `NodeBase`: the ones the class declares, the ones it inherits from an Oak base class of its own,
/// such as `Fields` that `ExprRecordNode` gets from `ExprRecordBaseNode`, and the ones of the Oak's
/// own interfaces it implements, such as `ITypeDefn.Members`, which are explicit implementations
/// and no public property of the class.
let declaredProperties (nodeClass: System.Type) : PropertyInfo list =
    let ownInterfaces: PropertyInfo array =
        nodeClass.GetInterfaces()
        |> Array.filter (fun (i: System.Type) -> i.DeclaringType = syntaxOakModule && i <> typeof<Node>)
        |> Array.collect (fun (i: System.Type) -> i.GetProperties())

    let rec ownClasses (t: System.Type) : System.Type list =
        if isNull t || t = typeof<NodeBase> || t.DeclaringType <> syntaxOakModule then
            []
        else
            t :: ownClasses t.BaseType

    ownClasses nodeClass
    |> Array.ofList
    |> Array.collect (fun (t: System.Type) ->
        t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
    )
    |> Array.append ownInterfaces
    |> Array.filter (fun (property: PropertyInfo) -> property.GetIndexParameters().Length = 0)
    |> Array.distinctBy (fun (property: PropertyInfo) -> property.Name)
    |> Array.toList

/// Every union case the Oak holds, as the union and the case's name: `Expr.Lambda` is there when
/// some node holds that value, directly or in an option, a list, a tuple or a `Choice`. A node class
/// can sit inside another node as well, as `ExprParenLambdaNode` holds an `ExprLambdaNode`, so only
/// the union value says which case is there.
let unionCases (oak: Oak) : (System.Type * string) list =
    let rec casesIn (declared: System.Type) (value: obj) : (System.Type * string) list =
        if isNull value then
            []
        elif isList declared then
            let element: System.Type = declared.GetGenericArguments()[0]

            value :?> System.Collections.IEnumerable
            |> Seq.cast<obj>
            |> Seq.toList
            |> List.collect (casesIn element)
        elif FSharpType.IsTuple declared then
            Array.zip (FSharpType.GetTupleElements declared) (FSharpValue.GetTupleFields value)
            |> Array.toList
            |> List.collect (fun (element: System.Type, item: obj) -> casesIn element item)
        elif FSharpType.IsUnion(declared, true) then
            // What its case holds as well: `ModuleDecl.TypeDefn` holds a `TypeDefn`, an option or a
            // `Choice` the value inside.
            let case, fields = FSharpValue.GetUnionFields(value, declared, true)

            let inside: (System.Type * string) list =
                Array.zip (case.GetFields()) fields
                |> Array.toList
                |> List.collect (fun (field: PropertyInfo, item: obj) -> casesIn field.PropertyType item)

            if declared.DeclaringType = syntaxOakModule then
                (declared, case.Name) :: inside
            else
                inside
        else
            []

    visits oak
    |> List.collect (fun (visit: Visit) ->
        declaredProperties (visit.Node.GetType())
        |> List.collect (fun (property: PropertyInfo) -> casesIn property.PropertyType (property.GetValue visit.Node))
    )
    |> List.distinct

/// The properties whose shape the report follows: optional parts and lists of parts.
let shapeProperties (nodeClass: System.Type) : PropertyInfo list =
    declaredProperties nodeClass
    |> List.filter (fun (property: PropertyInfo) -> isOption property.PropertyType || isList property.PropertyType)

/// The shape one property of one node has: `Some` or `None` for an optional part, `none`, `one` or
/// `several` for a list of parts.
let shapeOf (property: PropertyInfo) (node: Node) : string option =
    let value: obj =
        try
            property.GetValue node
        with _ ->
            null

    if isOption property.PropertyType then
        Some(if isNull value then "None" else "Some")
    elif isNull value then
        None
    else

    // One part and several lay out differently often enough to be told apart: a union with a single
    // case, a record with one field, a match with one clause.
    let enumerator: IEnumerator = (value :?> IEnumerable).GetEnumerator()

    if not (enumerator.MoveNext()) then Some "none"
    elif not (enumerator.MoveNext()) then Some "one"
    else Some "several"

/// The properties of a node class that hold one token, alone or optionally.
let tokenProperties (nodeClass: System.Type) : PropertyInfo list =
    declaredProperties nodeClass
    |> List.filter (fun (property: PropertyInfo) ->
        property.PropertyType = typeof<SingleTextNode>
        || property.PropertyType = typeof<SingleTextNode option>
    )

/// The token slot a node fills in its parent, named after the parent's property that holds it:
/// `(whole node)` for a node that is not a token of its parent, `(token)` for a token the parent holds
/// some other way, inside a list for instance.
let slotOf (visit: Visit) : System.Type * string =
    match visit.Node, visit.Parent with
    | :? SingleTextNode as token, Some parent ->
        let holder: PropertyInfo option =
            tokenProperties (parent.GetType())
            |> List.tryFind (fun (property: PropertyInfo) ->
                match property.GetValue parent with
                | :? SingleTextNode as held -> Object.ReferenceEquals(held, token)
                | :? (SingleTextNode option) as held ->
                    held
                    |> Option.exists (fun (held: SingleTextNode) -> Object.ReferenceEquals(held, token))
                | _ -> false
            )

        match holder with
        | Some property -> parent.GetType(), property.Name
        | None -> parent.GetType(), "(token)"
    | node, _ -> node.GetType(), "(whole node)"
