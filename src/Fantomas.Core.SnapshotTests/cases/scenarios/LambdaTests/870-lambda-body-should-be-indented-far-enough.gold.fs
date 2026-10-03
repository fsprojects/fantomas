let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
        eventEnvelope.Event
        |> projection.Update projectionState
    |> fun newState ->
        state
        |> Map.add eventEnvelope.Metadata.Source newState

let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
        eventEnvelope.Event
        |> projection.Update projectionState
    |> fun newState ->
        state
        |> Map.add eventEnvelope.Metadata.Source newState
