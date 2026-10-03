(*---
max_line_length = 80
fsharp_max_function_binding_width = 120
---*)
type SomeType() =
    member SomeMember(looooooooooooooooooooooooooooooooooong1: A, looooooooooooooooooooooooooooooooooong2: A) : string =
        printfn "a"
        "a"

    member SomeOtherMember () =
        printfn "b"
