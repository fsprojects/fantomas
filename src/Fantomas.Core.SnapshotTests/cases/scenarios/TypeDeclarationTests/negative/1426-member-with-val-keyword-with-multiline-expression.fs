type public Foo() =

    // Here it generates valid code
    static member FooBarThing1 =
        new TypedThingDefinition(
            "StringA",
            SomeLongThing.SomeProperty,
            IsMandatory = new Nullable<bool>(true),
            Blablablabla = moreStuff
        )

    // With "member val" it generates invalid code
    static member val FooBarThing2 =
        new TypedThingDefinition(
            "StringA",
            SomeLongThing.SomeProperty,
            IsMandatory = new Nullable<bool>(true),
            Blablablabla = moreStuff
        )
