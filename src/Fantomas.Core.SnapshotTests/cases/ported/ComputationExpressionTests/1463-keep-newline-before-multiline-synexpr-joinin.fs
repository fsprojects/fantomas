(*---
fsharp_max_infix_operator_expression = 40
fsharp_multiline_bracket_style = cramped
---*)
aggregateResult {
    apply id in someFunction
    also displayableId in AggregateResult.map (fun x -> string x.Z) g
    also person in getThing y |> AggregateResult.ofResult
    also more in AggregateResult.bind
                     (getLongfunctionNameWithLotsOfStuff
                      >> AggregateResult.ofResult)
                     mainThingThatHappens

    return
        { Id = id
          DisplayableId = displayableId
          More = more }
}

aggregateResult {
    apply id in someFunction
    also displayableId in AggregateResult.map (fun x -> string x.Z) g
    also person in getThing y |> AggregateResult.ofResult

    also more in AggregateResult.bind
                     (getLongfunctionNameWithLotsOfStuff
                      >> AggregateResult.ofResult)
                     mainThingThatHappens

    return
        { Id = id
          DisplayableId = displayableId
          More = more }
}
