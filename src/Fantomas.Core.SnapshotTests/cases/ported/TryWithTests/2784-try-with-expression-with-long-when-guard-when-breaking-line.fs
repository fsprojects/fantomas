(*---
indent_size = 2
---*)
try
  c ()
with
| :? WebSocketException as e when e.WebSocketErrorCode = WebSocketError.ConnectionClosedPrematurely && sourceParty = Agent ->
  ()
