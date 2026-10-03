let shrinkInput input =
    match toSynExprs input with
    | [] ->
        //stdout.WriteLine("Can't shrink {0} further.", sprintf "%A" input)
        Seq.empty
