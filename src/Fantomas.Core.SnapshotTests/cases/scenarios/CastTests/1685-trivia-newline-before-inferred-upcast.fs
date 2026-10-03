(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_multiline_bracket_style = cramped
---*)
namespace Blah

module Foo =

    let foo =
        { new IDisposable with
            member __.Dispose () =
                do ()

                upcast ()
        }
