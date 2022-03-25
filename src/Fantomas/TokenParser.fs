module internal Fantomas.TokenParser // TODO: rename module

open FSharp.Compiler.SyntaxTrivia
open Fantomas
open Fantomas.TokenParserBoolExpr
open Fantomas.TriviaTypes

let private getIndividualDefine (hashDirectives: ConditionalDirectiveTrivia list) : string list list =
    let rec visit (expr: IfDirectiveExpression) : string list =
        match expr with
        | IfDirectiveExpression.Not expr -> visit expr
        | IfDirectiveExpression.And (e1, e2)
        | IfDirectiveExpression.Or (e1, e2) -> visit e1 @ visit e2
        | IfDirectiveExpression.Ident s -> List.singleton s

    hashDirectives
    |> List.collect (function
        | ConditionalDirectiveTrivia.If (expr, _r) -> visit expr
        | _ -> [])
    |> List.distinct
    |> List.map List.singleton

let private getDefineExprs (hashDirectives: ConditionalDirectiveTrivia list) =
    let result =
        (([], []), hashDirectives)
        ||> List.fold (fun (contextExprs, exprAcc) hashLine ->
            let contextExpr e =
                e :: contextExprs
                |> List.reduce (fun x y -> IfDirectiveExpression.And(x, y))

            match hashLine with
            | ConditionalDirectiveTrivia.If (expr, _) -> expr :: contextExprs, contextExpr expr :: exprAcc
            | ConditionalDirectiveTrivia.Else _ ->
                contextExprs,
                IfDirectiveExpression.Not(
                    contextExprs
                    |> List.reduce (fun x y -> IfDirectiveExpression.And(x, y))
                )
                :: exprAcc
            | ConditionalDirectiveTrivia.EndIf _ -> List.tail contextExprs, exprAcc)
        |> snd
        |> List.rev

    result

let private getOptimizedDefinesSets (hashDirectives: ConditionalDirectiveTrivia list) =
    let maxSteps = FormatConfig.satSolveMaxStepsMaxSteps
    let defineExprs = getDefineExprs hashDirectives

    match mergeBoolExprs maxSteps defineExprs
          |> List.map snd
        with
    | [] -> [ [] ]
    | xs -> xs

let getDefineCombination (hashDirectives: ConditionalDirectiveTrivia list) : DefineCombination list =
    [ yield [] // always include the empty defines set
      yield! getOptimizedDefinesSets hashDirectives
      yield! getIndividualDefine hashDirectives ]
    |> List.distinct
