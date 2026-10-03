let compareThings (first : Thing) (second : Thing) =
    first =
        { second with
            Foo = first.Foo
            Bar = first.Bar
        }
