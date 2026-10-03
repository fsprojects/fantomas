(*---
fsharp_record_multiline_formatter = number_of_items
---*)
type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) =
        { inherit CommunicationUnsuccessfulException(message) }