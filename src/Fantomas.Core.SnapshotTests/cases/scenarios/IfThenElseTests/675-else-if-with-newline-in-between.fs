(*---
fsharp_max_if_then_else_short_width = 100
---*)
namespace Fantomas

module String =
    let merge a b =
            if la <> lb then
                if la > lb then a' else b'
            else
                if String.length a' < String.length b' then a' else b'
