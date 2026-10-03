(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
      module Foo =
          let someFunction { Firstname = fn; Lastname = ln; Age = age } =
              printfn "Name: %s" fn
              printfn "Last Name: %s" ln
              printfn "Age: %i" age
