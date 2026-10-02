(*---
fsharp_max_record_width = 10
fsharp_multiline_bracket_style = cramped
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
