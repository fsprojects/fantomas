module Foo =
    let blah () =
        match foo with
        | Thing crate ->

        crate.Apply
            { new Evaluator<_, _> with
                member __.Eval inner teq =
                    let foo =
                        // blah
                        let exists =
                            try
                                let defaultTime = (DateTime.FromFileTimeUtc 0L).ToLocalTime()

                                foo.CreationTime <> defaultTime
                            with
                            // hmm
                            | :? FileNotFoundException ->
                                false

                        exists

                    ()
            }
