let readModel
  (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
  (initState : 'State)
  : ReadModel<'Event, 'State>
  =
  ()
