(*---
fsharp_space_before_uppercase_invocation = true
---*)
module SomeModule =
    let DoSomething (a:SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
