module Fantomas.TokenParserBoolExpr

#nowarn "40"


[<RequireQualifiedAccess>]
type BoolExpr =
    | Ident of string
    | And of BoolExpr * BoolExpr
    | Or of BoolExpr * BoolExpr
    | Not of BoolExpr

module BoolExpr =
    let rec map f e =
        match f e with
        | Some x -> x
        | None ->
            match e with
            | BoolExpr.Not e -> BoolExpr.Not(map f e)
            | BoolExpr.And(e1, e2) -> BoolExpr.And(map f e1, map f e2)
            | BoolExpr.Or(e1, e2) -> BoolExpr.Or(map f e1, map f e2)
            | _ -> e

    let rec forall f e =
        f e && match e with
               | BoolExpr.Not e -> forall f e
               | BoolExpr.And(e1, e2)
               | BoolExpr.Or(e1, e2) -> forall f e1 && forall f e2
               | _ -> true

    let normalizeCNF expr =
        let mapUntilNotChanged mapFunctions expr =
            let oneStep e = mapFunctions |> Seq.fold (fun e f -> map f e) e
            expr
            |> Seq.unfold (fun e ->
                let e' = oneStep e
                if e = e' then None
                else Some(e', e'))
            |> Seq.tryLast
            |> Option.defaultValue expr

        let doubleNegative =
            function
            | BoolExpr.Not(BoolExpr.Not(e)) -> Some e
            | _ -> None

        let deMorgan =
            function
            | BoolExpr.Not(BoolExpr.And(e1, e2)) -> Some(BoolExpr.Or(BoolExpr.Not e1, BoolExpr.Not e2))
            | BoolExpr.Not(BoolExpr.Or(e1, e2)) -> Some(BoolExpr.And(BoolExpr.Not e1, BoolExpr.Not e2))
            | _ -> None

        let expandOr =
            function
            | BoolExpr.Or(e1, BoolExpr.And(e2, e3))
            | BoolExpr.Or(BoolExpr.And(e2, e3), e1) -> Some(BoolExpr.And(BoolExpr.Or(e1, e2), BoolExpr.Or(e1, e3)))
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
            | BoolExpr.And(e1, e2) -> toAndList e1 @ toAndList e2
            | e -> [ e ]

        let rec toOrList =
            function
            | BoolExpr.Or(e1, e2) -> toOrList e1 @ toOrList e2
            | e -> [ e ]

        let splitByNeg xs =
            xs
            |> List.map (function
                | BoolExpr.Not(BoolExpr.Ident x) -> Negative x
                | BoolExpr.Ident x -> Positive x
                | _ -> failwithf "Expr not in CNF: %A" e)
            |> set

        e
        |> toAndList
        |> List.map (toOrList >> splitByNeg)

    let eval cnf vals =
        let vals = set vals

        let evalTerm s =
            Set.intersect vals s
            |> Set.isEmpty
            |> not
        cnf |> List.forall evalTerm

    let trySolveSAT maxSteps cnf =
        let allLiterals =
            cnf
            |> Seq.collect id
            |> Seq.toList

        let groupedLiterals ls =
            ls
            |> Seq.groupBy (function
                | Positive x
                | Negative x -> x)
            |> Seq.map (fun (key, g) ->
                key,
                g
                |> Seq.distinct
                |> Seq.toList)
            |> Seq.toList

        let enforcedLit =
            cnf
            |> List.filter (fun t -> Set.count t = 1)
            |> List.collect Set.toList

        if groupedLiterals enforcedLit |> Seq.exists (fun (_, g) -> List.length g > 1) then Unsatisfiable
        else
            let (singletons, toSolve) =
                groupedLiterals allLiterals |> List.partition (fun (_, g) -> List.length g = 1)
            let singletonsLit = singletons |> List.collect snd
            let solvedSet = set (enforcedLit @ singletonsLit)
            let toSolve =
                toSolve
                |> List.filter
                    (fun (k, _) -> not (Set.contains (Positive k) solvedSet || Set.contains (Negative k) solvedSet))

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
                    if i > maxSteps then Some Unconclusive
                    else
                        let vals = vals @ comb
                        if eval cnf vals then
                            Some
                                (Satisfiable
                                    (vals
                                     |> List.choose (function
                                         | Positive x -> Some x
                                         | _ -> None)))
                        else None)
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
            BoolExpr.And(e1, e2) |> solve //|> Dbg.tee (fun r -> printfn "%A: %A" (BoolExpr.And(e1, e2)) r)

        let allPairs xs =
            xs
            |> Seq.mapi (fun i x ->
                xs
                |> Seq.mapi (fun j y ->
                    if i < j then Some(x, y)
                    else None))
            |> Seq.collect id
            |> Seq.choose id

        let exprs = exprs |> List.map (fun x -> x, None)

        let rec f exprs =
            let exprsIndexed = exprs |> Seq.mapi (fun i x -> i, x)
            match exprsIndexed
                  |> allPairs
                  |> Seq.tryPick
                      (fun ((i, (x, _)), (j, (y, _))) -> pairSolve x y |> Option.map (fun r -> (i, x), (j, y), r)) with
            | None -> exprs
            | Some((i, x), (j, y), r) ->
                f
                    ((exprsIndexed
                      |> Seq.filter (fun (k, _) -> i <> k && j <> k)
                      |> Seq.map snd
                      |> Seq.toList)
                     @ [ BoolExpr.And(x, y), Some r ])

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

module BoolExprParser =
    let (|Eq|_|) x y =
        if x = y then Some()
        else None

    let (|TakeUntil|_|) x xs =
        match List.takeWhile ((<>) x) xs with
        | y when y = xs -> None
        | y -> Some(y, List.skipWhile ((<>) x) xs)

    let (|ListSurround|_|) before after xs =
        let rec f d acc xs =
            match xs with
            | _ when d < 0 -> None
            | Eq before :: rest -> f (d + 1) (before :: acc) rest
            | [ Eq after ] when d = 1 -> List.rev acc |> Some
            | Eq after :: rest -> f (d - 1) (after :: acc) rest
            | x :: rest -> f d (x :: acc) rest
            | _ -> None
        match xs with
        | Eq before :: rest -> f 1 [] rest
        | _ -> None

    let (|ListSplit|_|) split xs =
        match xs with
        | TakeUntil split (x1, (_ :: x2)) -> Some(x1, x2)
        | _ -> None

    let (|ListSplitPick|_|) split f xs =
        let rec loop prev xs =
            seq {
                match xs with
                | TakeUntil split (x1, (_ :: x2)) ->
                    yield (prev @ x1, x2)
                    yield! loop (prev @ x1 @ [ split ]) x2
                | _ -> ()
            }
        loop [] xs |> Seq.tryPick f

    let rec (|SubExpr|_|) =
        function
        | ListSurround "(" ")" (ExprPat e) -> Some e
        | _ -> None

    and (|AndExpr|_|) =
        let chooser =
            function
            | (ExprPat e1, ExprPat e2) -> Some(e1, e2)
            | _ -> None
        function
        | ListSplitPick "&&" chooser (e1, e2) -> Some(BoolExpr.And(e1, e2))
        | _ -> None

    and (|OrExpr|_|) =
        let chooser =
            function
            | (ExprPat e1, ExprPat e2) -> Some(e1, e2)
            | _ -> None
        function
        | ListSplitPick "||" chooser (e1, e2) -> Some(BoolExpr.Or(e1, e2))
        | _ -> None

    and (|NotSubExpr|_|) =
        function
        | "!" :: SubExpr e -> Some(BoolExpr.Not e)
        | _ -> None

    and (|NotIdentExpr|_|) =
        function
        | "!" :: [x] -> Some(BoolExpr.Not(BoolExpr.Ident x))
        | _ -> None

    and (|ExprPat|_|) =
        function
        | NotSubExpr e
        | SubExpr e
        | AndExpr e
        | OrExpr e
        | NotIdentExpr e -> Some e
        | [x] -> BoolExpr.Ident x |> Some
        | _ -> None

    let parse =
        function
        | [] -> None
        | ExprPat e -> Some e
        | s -> failwithf "Fail to parse bool expr in #if: %A" s
