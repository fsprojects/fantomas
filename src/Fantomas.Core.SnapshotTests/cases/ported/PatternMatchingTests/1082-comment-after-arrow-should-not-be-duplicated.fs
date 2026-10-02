List.tryFind(fun { Type = t; Range = r }  ->
                    match t with
                    | MainNode SynMemberDefn_Member
                    | MainNode SynMemberSig_Member -> // trying to get AST trivia
                        RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                    | Token(MEMBER, _) -> // trying to get token trivia
                        r.StartLine = rangeOfBindingAndRhs.StartLine

                    | _ -> false
                )
