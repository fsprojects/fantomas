module Fantomas.DefineCombinationSolver

#nowarn "40"

open FSharp.Compiler.SyntaxTrivia

let rec private map f e =
    match f e with
    | Some x -> x
    | None ->
        match e with
        | IfDirectiveExpression.Not e -> IfDirectiveExpression.Not(map f e)
        | IfDirectiveExpression.And (e1, e2) -> IfDirectiveExpression.And(map f e1, map f e2)
        | IfDirectiveExpression.Or (e1, e2) -> IfDirectiveExpression.Or(map f e1, map f e2)
        | _ -> e

let rec private forall f e =
    f e
    && match e with
       | IfDirectiveExpression.Not e -> forall f e
       | IfDirectiveExpression.And (e1, e2)
       | IfDirectiveExpression.Or (e1, e2) -> forall f e1 && forall f e2
       | _ -> true

let rec private expressionsAreEquals (e1: IfDirectiveExpression) (e2: IfDirectiveExpression) : bool =
    match e1, e2 with
    | IfDirectiveExpression.Ident i1, IfDirectiveExpression.Ident i2 -> i1 = i2
    | IfDirectiveExpression.Not n1, IfDirectiveExpression.Not n2 -> expressionsAreEquals n1 n2
    | IfDirectiveExpression.And (a1, a2), IfDirectiveExpression.And (b1, b2) ->
        expressionsAreEquals a1 b1
        && expressionsAreEquals a2 b2
    | IfDirectiveExpression.Or (a1, a2), IfDirectiveExpression.Or (b1, b2) ->
        expressionsAreEquals a1 b1
        && expressionsAreEquals a2 b2
    | _ -> false

let private normalizeCNF expr =
    let mapUntilNotChanged mapFunctions expr =
        let oneStep e =
            mapFunctions |> Seq.fold (fun e f -> map f e) e

        expr
        |> Seq.unfold (fun e ->
            let e' = oneStep e

            if expressionsAreEquals e e' then
                None
            else
                Some(e', e'))
        |> Seq.tryLast
        |> Option.defaultValue expr

    let doubleNegative =
        function
        | IfDirectiveExpression.Not (IfDirectiveExpression.Not e) -> Some e
        | _ -> None

    let deMorgan =
        function
        | IfDirectiveExpression.Not (IfDirectiveExpression.And (e1, e2)) ->
            Some(IfDirectiveExpression.Or(IfDirectiveExpression.Not e1, IfDirectiveExpression.Not e2))
        | IfDirectiveExpression.Not (IfDirectiveExpression.Or (e1, e2)) ->
            Some(IfDirectiveExpression.And(IfDirectiveExpression.Not e1, IfDirectiveExpression.Not e2))
        | _ -> None

    let expandOr =
        function
        | IfDirectiveExpression.Or (e1, IfDirectiveExpression.And (e2, e3))
        | IfDirectiveExpression.Or (IfDirectiveExpression.And (e2, e3), e1) ->
            Some(IfDirectiveExpression.And(IfDirectiveExpression.Or(e1, e2), IfDirectiveExpression.Or(e1, e3)))
        | _ -> None

    expr
    |> mapUntilNotChanged [ doubleNegative
                            deMorgan
                            expandOr ]

type Literal =
    | Positive of string
    | Negative of string

type SATSolveResult =
    | Satisfiable of string list
    | Unsatisfiable
    | Unconclusive
// result: list of AND-connected terms; term - OR-connected Literals
let private toFlatCNF expr =
    let e = expr |> normalizeCNF

    let rec toAndList =
        function
        | IfDirectiveExpression.And (e1, e2) -> toAndList e1 @ toAndList e2
        | e -> [ e ]

    let rec toOrList =
        function
        | IfDirectiveExpression.Or (e1, e2) -> toOrList e1 @ toOrList e2
        | e -> [ e ]

    let splitByNeg xs =
        xs
        |> List.map (function
            | IfDirectiveExpression.Not (IfDirectiveExpression.Ident x) -> Negative x
            | IfDirectiveExpression.Ident x -> Positive x
            | _ -> failwithf "Expr not in CNF: %A" e)
        |> set

    e
    |> toAndList
    |> List.map (toOrList >> splitByNeg)

let private eval cnf vals =
    let vals = set vals

    let evalTerm s =
        Set.intersect vals s |> Set.isEmpty |> not

    cnf |> List.forall evalTerm

let private trySolveSAT maxSteps cnf =
    let allLiterals = cnf |> Seq.collect id |> Seq.toList

    let groupedLiterals ls =
        ls
        |> Seq.groupBy (function
            | Positive x
            | Negative x -> x)
        |> Seq.map (fun (key, g) -> key, g |> Seq.distinct |> Seq.toList)
        |> Seq.toList

    let enforcedLit =
        cnf
        |> List.filter (fun t -> Set.count t = 1)
        |> List.collect Set.toList

    if groupedLiterals enforcedLit
       |> Seq.exists (fun (_, g) -> List.length g > 1) then
        Unsatisfiable
    else
        let singletons, toSolve =
            groupedLiterals allLiterals
            |> List.partition (fun (_, g) -> List.length g = 1)

        let singletonsLit = singletons |> List.collect snd
        let solvedSet = set (enforcedLit @ singletonsLit)

        let toSolve =
            toSolve
            |> List.filter (fun (k, _) ->
                not (
                    Set.contains (Positive k) solvedSet
                    || Set.contains (Negative k) solvedSet
                ))

        let rec genCombinations groups =
            seq {
                match groups with
                | [] -> yield []
                | g :: gs ->
                    yield!
                        genCombinations gs
                        |> Seq.collect (fun l -> g |> Seq.map (fun x -> x :: l))
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
        |> Seq.mapi (fun i x ->
            xs
            |> Seq.mapi (fun j y -> if i < j then Some(x, y) else None))
        |> Seq.collect id
        |> Seq.choose id

    let exprs = exprs |> List.map (fun x -> x, None)

    let rec f exprs =
        let exprsIndexed = exprs |> Seq.mapi (fun i x -> i, x)

        match exprsIndexed
              |> allPairs
              |> Seq.tryPick (fun ((i, (x, _)), (j, (y, _))) ->
                  pairSolve x y
                  |> Option.map (fun r -> (i, x), (j, y), r))
            with
        | None -> exprs
        | Some ((i, x), (j, y), r) ->
            f (
                (exprsIndexed
                 |> Seq.filter (fun (k, _) -> i <> k && j <> k)
                 |> Seq.map snd
                 |> Seq.toList)
                @ [ IfDirectiveExpression.And(x, y), Some r ]
            )

    let r =
        f exprs
        |> List.choose (fun (e, r) ->
            r
            |> Option.orElseWith (fun () -> solve e)
            |> Option.map (fun x -> e, x))

    r

let solveDefinesForExpr maxSolveSteps e =
    e
    |> toFlatCNF
    |> trySolveSAT maxSolveSteps
    |> function
        | Satisfiable x -> Some x
        | _ -> None

/// Determine the result of an expression for a given set of defines
let solveExprForDefines (expr: IfDirectiveExpression) (defines: string seq) : bool =
    let defines = Set(defines)

    let rec visit (expr: IfDirectiveExpression) (continuation: bool -> bool) : bool =
        match expr with
        | IfDirectiveExpression.Not e -> visit e (not >> continuation)
        | IfDirectiveExpression.And (e1, e2) -> visit e1 (fun r1 -> visit e2 (fun r2 -> (r1 && r2) |> continuation))
        | IfDirectiveExpression.Or (e1, e2) -> visit e1 (fun r1 -> visit e2 (fun r2 -> (r1 || r2) |> continuation))
        | IfDirectiveExpression.Ident s -> defines.Contains s |> continuation

    visit expr id
