if kind = shiftFlag then
    (if errorSuppressionCountDown > 0 then
         errorSuppressionCountDown <- errorSuppressionCountDown - 1
#if DEBUG
         if Flags.debug then
             Console.WriteLine("shifting, reduced errorRecoveryLevel to {0}\n", errorSuppressionCountDown)
#endif
     let nextState = actionValue action

     if not haveLookahead then
         failwith "shift on end of input!"

     let data = tables.dataOfToken lookaheadToken
     valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos))
     stateStack.Push(nextState)
#if DEBUG
     if Flags.debug then
         Console.WriteLine(
             "shift/consume input {0}, shift to state {1}",
             report haveLookahead lookaheadToken,
             nextState
         )
#endif
     haveLookahead <- false

    )
