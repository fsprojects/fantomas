module internal Fantomas.SourceTransformer

open Fantomas.Context
open Fantomas.SourceCounter
open Fantomas.SourceParser

[<RequireQualifiedAccess>]
module List = 
    let inline atMostOne xs =
        match xs with
        | [] | [_] -> true
        | _ -> false

/// Check if the expression already has surrounding parentheses
let hasParenthesis = function
    | Paren _
    | ConstExpr(Const "()", _)
    | Tuple _ -> true
    | _ -> false

let isArrayOrList = function | ArrayOrList _ -> true | _ -> false

let hasParenInPat = function
    | PatParen _ -> true
    | _ -> false

let getByLookup range f x =
    fun ctx -> 
        if ctx.Config.StrictMode then
            f x ctx
        else
            match lookup range ctx with
            | Some x' ->
                str x' ctx
            | None ->
                f x ctx

// A few active patterns for printing purpose

let rec (|DoExprAttributesL|_|) = function
    | DoExpr _ | Attributes _  as x::DoExprAttributesL(xs, ys) -> Some(x::xs, ys)
    | DoExpr _ | Attributes _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|HashDirectiveL|_|) = function
    | HashDirective _ as x::HashDirectiveL(xs, ys) -> Some(x::xs, ys)
    | HashDirective _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|SigHashDirectiveL|_|) = function
    | SigHashDirective _ as x::SigHashDirectiveL(xs, ys) -> Some(x::xs, ys)
    | SigHashDirective _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|ModuleAbbrevL|_|) = function
    | ModuleAbbrev _ as x::ModuleAbbrevL(xs, ys) -> Some(x::xs, ys)
    | ModuleAbbrev _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|SigModuleAbbrevL|_|) = function
    | SigModuleAbbrev _ as x::SigModuleAbbrevL(xs, ys) -> Some(x::xs, ys)
    | SigModuleAbbrev _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|OpenL|_|) = function
    | Open _ as x::OpenL(xs, ys) -> Some(x::xs, ys)
    | Open _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|SigOpenL|_|) = function
    | SigOpen _ as x::SigOpenL(xs, ys) -> Some(x::xs, ys)
    | SigOpen _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|MDOpenL|_|) = function
    | MDOpen _ as x::MDOpenL(xs, ys) -> Some(x::xs, ys)
    | MDOpen _ as x::ys -> Some([x], ys)
    | _ -> None

let rec (|SigValL|_|) = function
    | SigVal _ as x::SigValL(xs, ys) -> Some(x::xs, ys)
    | SigVal _ as x::ys -> Some([x], ys)
    | _ -> None

let (|SigMultilineModuleDecl|_|) = function
    | SigHashDirective _
    | SigModuleAbbrev _
    | SigVal _
    | SigOpen _ -> None
    | md -> Some md

let rec (|SigMultilineModuleDeclL|_|) = function
    | SigMultilineModuleDecl x::SigMultilineModuleDeclL(xs, ys) -> Some(x::xs, ys)
    | SigMultilineModuleDecl x::ys -> Some([x], ys)
    | _ -> None

/// Gather PropertyGetSet in one printing call. 
/// Assume that PropertySet comes right after PropertyGet.
let (|PropertyWithGetSet|_|) = function
    | PropertyBinding(_, _, _, _, MFProperty PropertyGet, PatLongIdent(_, s1, _, _), _) as b1::bs -> 
        match bs with
        | PropertyBinding(_, _, _, _, MFProperty PropertySet, PatLongIdent(_, s2, _, _), _) as b2::bs when s1 = s2 -> 
            Some((b1, b2), bs)
        | _ -> None
    | PropertyBinding(_, _, _, _, MFProperty PropertySet, PatLongIdent(_, s2, _, _), _) as b2::bs -> 
        match bs with
        | PropertyBinding(_, _, _, _, MFProperty PropertyGet, PatLongIdent(_, s1, _, _), _) as b1::bs when s1 = s2 -> 
            Some((b1, b2), bs)
        | _ -> None
    | _ -> None 

let (|PropertyWithGetSetMemberDefn|_|) = function
    | MDMember(x1)::MDMember(x2)::xs ->
        match [x1; x2] with
        | PropertyWithGetSet((x1, x2), []) -> Some((x1, x2), xs)
        | _ -> None
    | _ -> None

let addParenIfAutoNln (countAstNode:CountAstNode) synExpr f =
    let expr = f synExpr
    expressionFitsOnRestOfLine
        expr
        (ifElse (hasParenthesis synExpr) (sepOpenT +> expr +> sepCloseT) expr)
        countAstNode

let addParenForTupleWhen f synExpr ctx =
    let condition e =
        match e with
        |ElIf _
        | FSharp.Compiler.SyntaxTree.SynExpr.Lambda _ -> true
        |_ -> false // "if .. then .. else" have precedence over ","
    let expr = f synExpr
    ifElse (condition synExpr) (sepOpenT +> expr +> sepCloseT) expr ctx
