Log.Logger <- LoggerConfiguration<Foo>().Destructure.FSharpTypes().WriteTo.Console().CreateLogger()
