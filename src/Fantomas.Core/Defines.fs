namespace Fantomas.Core

open Fantomas.FCS.SyntaxTrivia
open Fantomas.Core

type internal DefineCombination =
    | DefineCombination of defines: string list

    member x.Value =
        match x with
        | DefineCombination defines -> defines

    static member Empty = DefineCombination([])

module private DefineCombinationSolver =
    let rec map f e =
        match f e with
        | Some x -> x
        | None ->
            match e with
            | IfDirectiveExpression.Not e -> IfDirectiveExpression.Not(map f e)
            | IfDirectiveExpression.And(e1, e2) -> IfDirectiveExpression.And(map f e1, map f e2)
            | IfDirectiveExpression.Or(e1, e2) -> IfDirectiveExpression.Or(map f e1, map f e2)
            | _ -> e

    let rec forall f e =
        f e
        && match e with
           | IfDirectiveExpression.Not e -> forall f e
           | IfDirectiveExpression.And(e1, e2)
           | IfDirectiveExpression.Or(e1, e2) -> forall f e1 && forall f e2
           | _ -> true

    let rec expressionsAreEquals (e1: IfDirectiveExpression) (e2: IfDirectiveExpression) : bool =
        match e1, e2 with
        | IfDirectiveExpression.Ident i1, IfDirectiveExpression.Ident i2 -> i1 = i2
        | IfDirectiveExpression.Not n1, IfDirectiveExpression.Not n2 -> expressionsAreEquals n1 n2
        | IfDirectiveExpression.And(a1, a2), IfDirectiveExpression.And(b1, b2) ->
            expressionsAreEquals a1 b1 && expressionsAreEquals a2 b2
        | IfDirectiveExpression.Or(a1, a2), IfDirectiveExpression.Or(b1, b2) ->
            expressionsAreEquals a1 b1 && expressionsAreEquals a2 b2
        | _ -> false

    let normalizeCNF expr =
        let mapUntilNotChanged mapFunctions expr =
            let oneStep e =
                mapFunctions |> Seq.fold (fun e f -> map f e) e

            expr
            |> Seq.unfold (fun e ->
                let e' = oneStep e
                if expressionsAreEquals e e' then None else Some(e', e'))
            |> Seq.tryLast
            |> Option.defaultValue expr

        let doubleNegative =
            function
            | IfDirectiveExpression.Not(IfDirectiveExpression.Not e) -> Some e
            | _ -> None

        let deMorgan =
            function
            | IfDirectiveExpression.Not(IfDirectiveExpression.And(e1, e2)) ->
                Some(IfDirectiveExpression.Or(IfDirectiveExpression.Not e1, IfDirectiveExpression.Not e2))
            | IfDirectiveExpression.Not(IfDirectiveExpression.Or(e1, e2)) ->
                Some(IfDirectiveExpression.And(IfDirectiveExpression.Not e1, IfDirectiveExpression.Not e2))
            | _ -> None

        let expandOr =
            function
            | IfDirectiveExpression.Or(e1, IfDirectiveExpression.And(e2, e3))
            | IfDirectiveExpression.Or(IfDirectiveExpression.And(e2, e3), e1) ->
                Some(IfDirectiveExpression.And(IfDirectiveExpression.Or(e1, e2), IfDirectiveExpression.Or(e1, e3)))
            | _ -> None

        expr |> mapUntilNotChanged [ doubleNegative; deMorgan; expandOr ]

    type Literal =
        | Positive of string
        | Negative of string

    type SATSolveResult =
        | Satisfiable of string list
        | Unsatisfiable
        | Unconclusive
    // result: list of AND-connected terms; term - OR-connected Literals
    let toFlatCNF expr =
        let e = expr |> normalizeCNF

        let rec toAndList =
            function
            | IfDirectiveExpression.And(e1, e2) -> toAndList e1 @ toAndList e2
            | e -> [ e ]

        let rec toOrList =
            function
            | IfDirectiveExpression.Or(e1, e2) -> toOrList e1 @ toOrList e2
            | e -> [ e ]

        let splitByNeg xs =
            xs
            |> List.map (function
                | IfDirectiveExpression.Not(IfDirectiveExpression.Ident x) -> Negative x
                | IfDirectiveExpression.Ident x -> Positive x
                | _ -> failwithf "Expr not in CNF: %A" e)
            |> set

        e |> toAndList |> List.map (toOrList >> splitByNeg)

    let eval cnf vals =
        let vals = set vals

        let evalTerm s =
            Set.intersect vals s |> Set.isEmpty |> not

        cnf |> List.forall evalTerm

    let trySolveSAT maxSteps cnf =
        let allLiterals = cnf |> Seq.collect id |> Seq.toList

        let groupedLiterals ls =
            ls
            |> Seq.groupBy (function
                | Positive x
                | Negative x -> x)
            |> Seq.map (fun (key, g) -> key, g |> Seq.distinct |> Seq.toList)
            |> Seq.toList

        let enforcedLit =
            cnf |> List.filter (fun t -> Set.count t = 1) |> List.collect Set.toList

        if groupedLiterals enforcedLit |> List.exists (fun (_, g) -> List.length g > 1) then
            Unsatisfiable
        else
            let singletons, toSolve =
                groupedLiterals allLiterals |> List.partition (fun (_, g) -> List.length g = 1)

            let singletonsLit = singletons |> List.collect snd
            let solvedSet = set (enforcedLit @ singletonsLit)

            let toSolve =
                toSolve
                |> List.filter (fun (k, _) ->
                    not (Set.contains (Positive k) solvedSet || Set.contains (Negative k) solvedSet))

            let rec genCombinations groups =
                seq {
                    match groups with
                    | [] -> yield []
                    | g :: gs -> yield! genCombinations gs |> Seq.collect (fun l -> g |> Seq.map (fun x -> x :: l))
                }

            let solve vals groups =
                let combinations = genCombinations (groups |> List.map snd)

                combinations
                |> Seq.mapi (fun i comb ->
                    if i > maxSteps then
                        Some Unconclusive
                    else
                        let vals = vals @ comb

                        if eval cnf vals then
                            Some(
                                Satisfiable(
                                    vals
                                    |> List.choose (function
                                        | Positive x -> Some x
                                        | _ -> None)
                                )
                            )
                        else
                            None)
                |> Seq.tryPick id
                |> Option.defaultValue Unsatisfiable

            solve (Seq.toList solvedSet) toSolve

    let mergeBoolExprs maxSolveSteps exprs =
        let solve e =
            e
            |> toFlatCNF
            |> trySolveSAT maxSolveSteps
            |> function
                | Satisfiable x -> Some x
                | _ -> None

        let pairSolve e1 e2 =
            IfDirectiveExpression.And(e1, e2) |> solve //|> Dbg.tee (fun r -> printfn "%A: %A" (IfDirectiveExpression.And(e1, e2)) r)

        let allPairs xs =
            xs
            |> Seq.mapi (fun i x -> xs |> Seq.mapi (fun j y -> if i < j then Some(x, y) else None))
            |> Seq.collect id
            |> Seq.choose id

        let exprs = exprs |> List.map (fun x -> x, None)

        let rec f exprs =
            let exprsIndexed = exprs |> Seq.mapi (fun i x -> i, x)

            match
                exprsIndexed
                |> allPairs
                |> Seq.tryPick (fun ((i, (x, _)), (j, (y, _))) ->
                    pairSolve x y |> Option.map (fun r -> (i, x), (j, y), r))
            with
            | None -> exprs
            | Some((i, x), (j, y), r) ->
                f (
                    (exprsIndexed
                     |> Seq.choose (fun (k, xs) -> if i <> k && j <> k then Some(xs) else None)
                     |> Seq.toList)
                    @ [ IfDirectiveExpression.And(x, y), Some r ]
                )

        let r =
            f exprs
            |> List.choose (fun (e, r) -> r |> Option.orElseWith (fun () -> solve e) |> Option.map (fun x -> e, x))

        r

module Defines =

    let getIndividualDefine (hashDirectives: ConditionalDirectiveTrivia list) : string list list =
        let rec visit (expr: IfDirectiveExpression) : string list =
            match expr with
            | IfDirectiveExpression.Not expr -> visit expr
            | IfDirectiveExpression.And(e1, e2)
            | IfDirectiveExpression.Or(e1, e2) -> visit e1 @ visit e2
            | IfDirectiveExpression.Ident s -> List.singleton s

        hashDirectives
        |> List.collect (function
            | ConditionalDirectiveTrivia.If(expr, _r) -> visit expr
            | _ -> [])
        |> List.distinct
        |> List.map List.singleton

    let getDefineExprs (hashDirectives: ConditionalDirectiveTrivia list) =
        let result =
            (([], []), hashDirectives)
            ||> List.fold (fun (contextExprs, exprAcc) hashLine ->
                let contextExpr e =
                    e :: contextExprs |> List.reduce (fun x y -> IfDirectiveExpression.And(x, y))

                match hashLine with
                | ConditionalDirectiveTrivia.If(expr, _) -> expr :: contextExprs, contextExpr expr :: exprAcc
                | ConditionalDirectiveTrivia.Else _ ->
                    contextExprs,
                    IfDirectiveExpression.Not(contextExprs |> List.reduce (fun x y -> IfDirectiveExpression.And(x, y)))
                    :: exprAcc
                | ConditionalDirectiveTrivia.EndIf _ -> List.tail contextExprs, exprAcc)
            |> snd
            |> List.rev

        result

    let satSolveMaxStepsMaxSteps = 100

    let getOptimizedDefinesSets (hashDirectives: ConditionalDirectiveTrivia list) =
        let defineExprs = getDefineExprs hashDirectives

        match
            DefineCombinationSolver.mergeBoolExprs satSolveMaxStepsMaxSteps defineExprs
            |> List.map snd
        with
        | [] -> [ [] ]
        | xs -> xs

    let getDefineCombination (hashDirectives: ConditionalDirectiveTrivia list) : DefineCombination list =
        [ yield [] // always include the empty defines set
          yield! getOptimizedDefinesSets hashDirectives
          yield! getIndividualDefine hashDirectives ]
        |> List.distinct
        |> List.map DefineCombination
