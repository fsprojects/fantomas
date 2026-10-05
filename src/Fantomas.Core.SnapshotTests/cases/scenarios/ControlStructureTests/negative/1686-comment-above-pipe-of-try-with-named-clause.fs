(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
namespace Foo

module Foo =
    let a =
        try
            failwith ""
        with
        // hi!
        | :? Exception as e ->
            failwith ""
