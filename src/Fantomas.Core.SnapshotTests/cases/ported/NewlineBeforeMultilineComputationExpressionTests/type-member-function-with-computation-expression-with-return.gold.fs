type Foo() =
    member this.Bar x : Task<unit> = task {
        // some computation here
        ()
    }
