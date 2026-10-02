try
  c ()
with :? WebSocketException as e when
    e.WebSocketErrorCode = WebSocketError.ConnectionClosedPrematurely
    && sourceParty = Agent ->
  ()
