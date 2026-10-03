module Foo =
    let bar () =
        let thing =
            Mock()
                .Setup(fun aaaaaaaaaaaa ->
                    <@ aaaaaaaaaaaa.Abcdefghijklmnopqrs "Food" "IsTastier" @>
                )
                .Returns(false)
                .Create()

        ()
