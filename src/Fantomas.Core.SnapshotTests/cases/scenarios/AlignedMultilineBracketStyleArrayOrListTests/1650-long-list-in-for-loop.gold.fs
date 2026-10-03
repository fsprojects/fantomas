module Foo =

    let foo () =
        let bar =
            seq {
                for i in
                    [
                        "hello1"
                        "hello1"
                        "hello1"
                        "hello1"
                        "hello1"
                    ] do
                    yield
                        i,
                        seq {
                            yield "hi"
                            yield "bye"
                        }
            }

        ()
