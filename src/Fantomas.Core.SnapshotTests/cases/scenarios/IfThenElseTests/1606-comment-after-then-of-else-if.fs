type internal Foo private () =
    static member Bar : int option =
        if thing = 1 then
            printfn "hi"
        else if
            veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong |> Seq.forall (fun (u : VeryVeryVeryVeryVeryVeryVeryLong) -> u.Length = 0) //
            then
              printfn "hi"
        else failwith ""

type internal Foo2 private () =
    static member Bar : int option =
        if thing = 1 then
            printfn "hi"
        else if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong
                |> Seq.forall (fun (u: VeryVeryVeryVeryVeryVeryVeryLong) -> u.Length = 0) //
        then
            printfn "hi"
        else
            failwith ""
