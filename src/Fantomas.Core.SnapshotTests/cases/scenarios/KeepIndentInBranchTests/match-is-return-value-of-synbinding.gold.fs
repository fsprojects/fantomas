module Foo =
    let make () =
        { new IBar with
            member __.DoIt(i: int) =
                doSomeStuff ()

                match i with
                | 0 ->
                    handleIt ()
                    None
                | _ ->

                let baz = foo ()

                match int baz with
                | 0 ->
                    handleAgain ()
                    None
                | _ ->

                doEvenMore ()
                Some 3 }
