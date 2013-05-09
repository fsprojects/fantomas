module internal Fantomas.SourceTransformer

open Fantomas.FormatConfig
open Fantomas.SourceParser

[<RequireQualifiedAccess>]
module List = 
    let inline atmostOne xs =
        match xs with
        | [] | [_] -> true
        | _ -> false

/// Check whether an expression should be broken into multiple lines. 
/// Notice that order of patterns matters due to non-disjoint property.
let rec multiline = function
    | ConstExpr _
    | NullExpr
    | OptVar _
    | SequentialSimple _ ->
        false

    | ObjExpr _
    | While _
    | For _
    | ForEach _
    | MatchLambda _
    | TryWith _
    | TryFinally _
    | Sequentials _
    | IfThenElse _ ->
        true

    | Paren e
    | SingleExpr(_, e)
    | TypedExpr(_, e, _)
    | CompExpr(_, e)
    | ArrayOrListOfSeqExpr(_, e)
    | DesugaredMatch(_, e)
    | Lambda(e, _)
    | TypeApp(e, _)
    | LongIdentSet(_, e)
    | DotGet(e, _)
    | TraitCall(_, _, e) ->
        multiline e

    | Quote(e1, e2, _)
    | JoinIn(e1, e2)
    | DotSet(e1, _, e2)
    | LetOrUseBang(_, _, e1, e2) ->
        multiline e1 || multiline e2

    | Tuple es ->
        List.exists multiline es

    // An infix app is multiline if it contains at least two new line infix ops
    | InfixApps(e, es) ->
        multiline e
        || not (List.atmostOne (List.filter (fst >> NewLineInfixOps.Contains) es))
        || List.exists (snd >> multiline) es
    
    | App(e1, es) ->
        multiline e1 || List.exists multiline es
    | DotIndexedGet(e, es) ->
        multiline e || List.exists multiline es

    | DotIndexedSet(e1, es, e2) ->
        multiline e1 || multiline e2 || List.exists multiline es

    | Match(e, cs) ->
        not (List.isEmpty cs) || multiline e
    | LetOrUse(_, _, bs, e) ->
        not (List.isEmpty bs) || multiline e

    // An array or a list is multiline if there are at least two elements
    | ArrayOrList(_, es) ->
        not (List.atmostOne es)

    // A record is multiline if there is at least two fields present
    | Record(xs, _) ->
        let fields = xs |> List.choose ((|RecordFieldName|) >> snd) 
        not (List.atmostOne fields) || List.exists multiline fields

    // Default mode is single-line
    | _ -> false

/// Check if the expression already has surrounding parentheses
let hasParenthesis = function
    | Paren _
    | ConstExpr(Const "()")
    | Tuple _ -> true
    | _ -> false

let hasParenInPat = function
    | PatParen _
    | PatConst(Const "()") -> true
    | _ -> false

let inline genConst c =
    match c with
    | Const c -> !- c
    | Unresolved c -> fun ctx -> str (content c ctx) ctx

// A few active patterns for printing purpose

let rec (|DoExprAttributesL|_|) = function
    | DoExpr _ | Attributes _  as x::DoExprAttributesL(xs, ys) -> Some(x::xs, ys)
    | DoExpr _ | Attributes _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|HashDirectiveL|_|) = function
    | HashDirective _ as x::HashDirectiveL(xs, ys) -> Some(x::xs, ys)
    | HashDirective _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|ModuleAbbrevL|_|) = function
    | ModuleAbbrev _ as x::ModuleAbbrevL(xs, ys) -> Some(x::xs, ys)
    | ModuleAbbrev _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|OpenL|_|) = function
    | Open _ as x::OpenL(xs, ys) -> Some(x::xs, ys)
    | Open _ as x::ys -> Some([x], ys)
    | _ -> None

let (|OneLinerExpr|_|) e =
    if multiline e then None else Some e

let (|OneLinerBinding|MultilineBinding|) b =
    match b with
    | LetBinding([], PreXmlDoc [||], _, _, _, _, OneLinerExpr _)
    | DoBinding([], PreXmlDoc [||], OneLinerExpr _)
    | MemberBinding([], PreXmlDoc [||], _, _, _, _, OneLinerExpr _)
    | PropertyBinding([], PreXmlDoc [||], _, _, _, _, OneLinerExpr _) 
    | ExplicitCtor([], PreXmlDoc [||], _, _, OneLinerExpr _) ->
        OneLinerBinding b

    | _ -> MultilineBinding b

let rec (|OneLinerLetL|_|) = function
    | Let(OneLinerBinding _) as x::OneLinerLetL(xs, ys) -> Some(x::xs, ys)
    | Let(OneLinerBinding _) as x::ys -> Some([x], ys)
    | _ -> None

let (|MultilineModuleDecl|_|) = function
    | DoExpr _
    | Attributes _
    | HashDirective _
    | ModuleAbbrev _
    | Open _
    | Let(OneLinerBinding _) -> None
    | md -> Some md

let rec (|MultilineModuleDeclL|_|) = function
    | MultilineModuleDecl x::MultilineModuleDeclL(xs, ys) -> Some(x::xs, ys)
    | MultilineModuleDecl x::ys -> Some([x], ys)
    | _ -> None

/// Gather PropertyGetSet in one printing call. 
/// Assume that PropertySet comes right after PropertyGet.
let (|PropertyWithGetSet|_|) = function
    | PropertyBinding(_, _, _, _, MFProperty PropertyGet, PatLongIdent(_, s1, _, _), _) as b1::bs -> 
        match bs with
        | PropertyBinding(_, _, _, _, MFProperty PropertySet, PatLongIdent(_, s2, _, _), _) as b2::bs when s1 = s2 -> 
            Some((b1, b2), bs)
        | _ -> None
    | _ -> None 

let (|PropertyWithGetSetMemberDefn|_|) = function
    | MDMember(x1)::MDMember(x2)::xs ->
        match [x1; x2] with
        | PropertyWithGetSet((x1, x2), []) -> Some((x1, x2), xs)
        | _ -> None
    | _ -> None

let (|OneLinerMemberDefn|MultilineMemberDefn|) md =
    match md with
    | MDImplicitInherit(_, OneLinerExpr _, _)
    | MDOpen _
    | MDInherit _
    | MDValField _
    | MDImplicitCtor _
    | MDMember(OneLinerBinding _)
    | MDInterface(_, None)
    | MDAutoProperty([], PreXmlDoc [||], _, _, OneLinerExpr _, _)
    | MDAbstractSlot([], PreXmlDoc [||], _, _, _, _, _)
    | MDLetBindings(_, _, [OneLinerBinding _]) ->
        OneLinerMemberDefn md

    | _ -> MultilineMemberDefn md

let rec (|OneLinerMemberDefnL|_|) xs = 
    match xs with
    /// This pattern prevents PropertyWithGetSet to be taken separately
    | PropertyWithGetSetMemberDefn _ -> Some([], xs)
    | OneLinerMemberDefn x::OneLinerMemberDefnL(xs, ys) -> Some(x::xs, ys)
    | OneLinerMemberDefn x::ys -> Some([x], ys)
    | _ -> None

type Data<'a, 'b> =
    | Pair of 'b * 'b
    | Single of 'a

/// Gather all multiline member definitions. 
/// This should be used before one-liner pattern.
let rec (|MultilineMemberDefnL|_|) = function
    | PropertyWithGetSetMemberDefn((x1, x2), MultilineMemberDefnL(xs, ys)) -> Some(Pair(x1, x2)::xs, ys)
    | PropertyWithGetSetMemberDefn((x1, x2), ys) -> Some([Pair(x1, x2)], ys)
    | MultilineMemberDefn x::MultilineMemberDefnL(xs, ys) -> Some(Single x::xs, ys)
    | MultilineMemberDefn x::ys -> Some([Single x], ys)
    | _ -> None

let rec (|OneLinerBindingL|_|) xs =
    match xs with
    | PropertyWithGetSet _ -> Some([], xs)
    | OneLinerBinding x::OneLinerBindingL(xs, ys) -> Some(x::xs, ys)
    | OneLinerBinding x::ys -> Some([x], ys)
    | _ -> None

/// Gather all multiline bindings. 
/// This should be used before one-liner pattern.
let rec (|MultilineBindingL|_|) = function
    | PropertyWithGetSet((x1, x2), MultilineBindingL(xs, ys)) -> Some(Pair(x1, x2)::xs, ys)
    | PropertyWithGetSet((x1, x2), ys) -> Some([Pair(x1, x2)], ys)
    | MultilineBinding x::MultilineBindingL(xs, ys) -> Some(Single x::xs, ys)
    | MultilineBinding x::ys -> Some([Single x], ys)
    | _ -> None
