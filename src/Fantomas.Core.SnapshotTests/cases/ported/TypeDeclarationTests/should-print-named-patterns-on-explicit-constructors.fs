type StateMachine(makeAsync) =
    new(fileName, makeAsync, initState) as secondCtor =
        new StateMachine(makeAsync)
        then
            secondCtor.Init(fileName, initState)
    