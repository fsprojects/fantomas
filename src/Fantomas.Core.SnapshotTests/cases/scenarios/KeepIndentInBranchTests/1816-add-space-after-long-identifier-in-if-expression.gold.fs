module Foo =
    let assertConsistent () : unit =
        if
            veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong
        then
            ()
        else if foo = bar then
            ()
        else

        let leftSet = HashSet(FooBarBaz.keys leftThings)

        leftSet.SymmetricExceptWith(FooBarBaz.keys rightThings)
        |> ignore
