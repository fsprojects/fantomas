(*---
fsharp_alternative_long_member_definitions = true
---*)
type StateMachine(
    // meh
) =
    new(
        // also meh but with an int
        x:int) as secondCtor = StateMachine()
