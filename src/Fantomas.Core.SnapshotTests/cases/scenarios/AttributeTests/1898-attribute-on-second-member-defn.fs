type Test1() =
  member x.Test() = ()

and Test2() =

  let someEvent = Event<EventHandler<int>, int>()

  [<CLIEvent>]
  member x.SomeEvent = someEvent.Publish
