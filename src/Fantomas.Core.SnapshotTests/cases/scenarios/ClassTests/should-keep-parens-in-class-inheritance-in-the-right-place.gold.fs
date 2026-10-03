type StateMachine(makeAsync) as this =
    class
        inherit DGMLClass()

        let functions = System.Collections.Generic.Dictionary<string, IState>()
    end
