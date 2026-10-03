module Test =
    let testFunc() =
        let someObject =
            someStaticObject.Create(
                ((fun o ->
                    o.SomeProperty <- ""
                    o.Role <- "Has to be at least two properties")))

        /// Comment can't be removed to reproduce bug
        let someOtherValue = ""

        someObject.someFunc "can't remove any of this stuff"
        someMutableProperty <- "not even this"