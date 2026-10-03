(*---
max_line_length = 80
---*)
namespace FSX.Infrastructure

module Unix =

    let GrabTheFirstStringBeforeTheFirstColon (lines: seq<string>) =
        seq {
            for line in lines do
                yield
                    (line.Split(
                        [| ":" |],
                        StringSplitOptions.RemoveEmptyEntries
                    )).[0]
        }
