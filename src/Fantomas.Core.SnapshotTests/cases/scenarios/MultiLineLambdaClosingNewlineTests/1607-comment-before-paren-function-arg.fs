(*---
max_line_length = 100
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
namespace Bar

[<RequireQualifiedAccess>]
module Foo =
    /// Blah
    let bang<'a when 'a : equality> (a : Foo<'a>) (ans : ('a * System.TimeSpan) list) : bool =
        List.length x = List.length y
        &&
        List.forall2
        //
          (fun (a, ta) (b, tb) -> a.Equals b && ta = tb)
          x
          y
