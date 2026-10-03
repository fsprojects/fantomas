if kind = shiftFlag then
    (if errorSuppressionCountDown > 0 then
         errorSuppressionCountDown <- errorSuppressionCountDown - 1
     #if DEBUG
     #endif
     let nextState = actionValue action

     if not haveLookahead then
         failwith "shift on end of input!"

     let data = tables.dataOfToken lookaheadToken
     valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos))
     stateStack.Push(nextState)
     #if DEBUG
     #endif
     haveLookahead <- false

    )
