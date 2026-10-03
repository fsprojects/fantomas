(*---
max_line_length = 50
---*)
let logger =
    Mock<ILogger>()
        .Setup(fun log -> <@ log.Log(error) @>)
        .Returns(())
        .Create()
