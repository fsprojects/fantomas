Assert.That(
    Assert.Throws(fun () -> FooFooFooFooFooFoo.BarBar.dodododo filesfiles [] outoutout |> ignore)
        .Message,
    Is.EqualTo(message)
)
