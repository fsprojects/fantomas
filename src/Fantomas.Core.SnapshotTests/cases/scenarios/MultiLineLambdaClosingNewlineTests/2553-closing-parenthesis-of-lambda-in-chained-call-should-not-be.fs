(*---
max_line_length = 100
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =
    let bar () =
        let thing =
            Mock()
                .Setup(fun aaaaaaaaaaaa -> <@ aaaaaaaaaaaa.Abcdefghijklmnopqrs "Food" "IsTastier" @>).Returns(false)
                .Create()
        ()
