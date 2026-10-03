(*---
fsharp_multiline_bracket_style = cramped
---*)
let test () =
    let something = "something"

    { new IDisposable with
        override this.Dispose() = dispose somethingElse }
