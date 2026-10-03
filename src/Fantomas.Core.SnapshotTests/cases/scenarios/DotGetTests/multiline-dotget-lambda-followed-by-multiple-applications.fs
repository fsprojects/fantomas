(*---
max_line_length = 80
---*)
mock
                .Setup(fun m ->
                // some comment
                m.CreateBlah
                    (It.IsAny<string>(), It.IsAny<string>(), It.IsAny<Id>(), It.IsAny<uint32>()))
                .Returns(Some mock)
                .OrNot()
                .End
