namespace SomeNamespace

module SomeModule =

    let SomeFunc () =
        let someLocalFunc someVeryLooooooooooooooooooooooooooooooooooooooooooooooooongParam =
            async {
                if (someVeryLooooooooooooooooooooooooooooooooooooooooooooooooongParam = 1) then
                    return failwith "xxx"
            }

        ()
