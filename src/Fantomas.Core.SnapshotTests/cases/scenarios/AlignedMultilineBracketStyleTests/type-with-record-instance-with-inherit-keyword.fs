(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) =
        { inherit CommunicationUnsuccessfulException(message) }