let genMemberFlagsForMemberBinding astContext (mf: MemberFlags) (rangeOfBindingAndRhs: range) =
    fun ctx ->
        match mf with
        | MFOverride _ ->
            (fun (ctx: Context) -> // trying to get AST trivia

                ctx.Trivia
                |> List.tryFind (fun { Type = t; Range = r } -> // trying to get token trivia

                    match t with
                    | MainNode "SynMemberDefn.Member" -> RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                    | Token { TokenInfo = { TokenName = "MEMBER" } } -> r.StartLine = rangeOfBindingAndRhs.StartLine

                    | _ -> false)
                |> Option.defaultValue (!- "override ")
                <| ctx)
        <| ctx
