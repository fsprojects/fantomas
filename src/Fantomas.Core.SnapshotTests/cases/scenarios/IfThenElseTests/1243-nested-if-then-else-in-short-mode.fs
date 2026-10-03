            let funcs =
                fse.MembersFunctionsAndValues
                |> Seq.sortWith (fun n1 n2 ->
                    let modifierScore (f : FSharpMemberOrFunctionOrValue) =
                        if f.IsProperty then
                            if f.IsInstanceMember then
                                if f.IsDispatchSlot then 9 else 1
                            else 8
                        elif f.IsMember then
                            if f.IsInstanceMember then
                                if f.IsDispatchSlot then 11 else 2
                            else 10
                        else 3
                    let n1Score = modifierScore n1
                    let n2Score = modifierScore n2
                    if n1Score = n2Score then
                        n1.DisplayName.CompareTo n2.DisplayName
                    else
                        n1Score.CompareTo n2Score
                )
