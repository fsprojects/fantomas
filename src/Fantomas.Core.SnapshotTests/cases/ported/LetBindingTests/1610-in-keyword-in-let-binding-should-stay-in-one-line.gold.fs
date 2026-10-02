module Foo =
    let bar () =

        let f1 = ()

        let runTest () =
            let (Thing f) = [ a ; b ] |> Blah.tryConcat |> Option.get in f () |> ignore

        Assert.Throws<exn> runTest |> ignore

    let bar2 () =

        let f1 = ()

        let runTest () =
            let (Thing f) = [ a ; b ] |> Blah.tryConcat |> Option.get in f () |> ignore

        Assert.Throws<exn> runTest |> ignore
