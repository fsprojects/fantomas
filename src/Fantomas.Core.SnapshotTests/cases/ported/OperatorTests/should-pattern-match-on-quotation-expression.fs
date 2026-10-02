let rec print expr =
    match expr with
    | SpecificCall <@@ (+) @@> (_, _, exprList) ->
        print exprList.Head
        printf " + "
        print exprList.Tail.Head
    | _ -> ()