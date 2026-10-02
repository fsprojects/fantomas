(*---
max_line_length = 30
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
      module Foo =
          let Bar () =
              if x then
                  match foo with
                  | { Bar = true
                      Baz = _ } -> failwith "xxx"
                  | _ -> None
