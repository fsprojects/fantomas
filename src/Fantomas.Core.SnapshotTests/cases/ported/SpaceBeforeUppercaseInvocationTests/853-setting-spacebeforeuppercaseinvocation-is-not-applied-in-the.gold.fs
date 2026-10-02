module SomeModule =
    let DoSomething (a: SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
