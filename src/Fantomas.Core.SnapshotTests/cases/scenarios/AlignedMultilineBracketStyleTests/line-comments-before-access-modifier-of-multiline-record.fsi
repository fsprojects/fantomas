(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_max_record_width = 10
---*)
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
            Barry: string
        }
