(*---
max_line_length = 119
---*)
type Class() =
    member this.``kk``() =
        async {
            mock
                .Setup(fun m ->
                m.CreateBlah
                    (It.IsAny<string>(), It.IsAny<string>(), It.IsAny<Id>(), It.IsAny<uint32>()))
                .Returns(Some mock)
                .End
        }
        |> Async.StartImmediate
