(*---
indent_size = 2
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_colon = true
fsharp_space_after_comma = false
fsharp_space_around_delimiter = false
fsharp_max_infix_operator_expression = 40
fsharp_max_function_binding_width = 60
---*)
let projectIntoMap projection =
    fun state eventEnvelope ->
      state
      |> Map.tryFind eventEnvelope.Metadata.Source
      |> Option.defaultValue projection.Init
      |> fun projectionState -> eventEnvelope.Event |> projection.Update projectionState
      |> fun newState -> state |> Map.add eventEnvelope.Metadata.Source newState

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
