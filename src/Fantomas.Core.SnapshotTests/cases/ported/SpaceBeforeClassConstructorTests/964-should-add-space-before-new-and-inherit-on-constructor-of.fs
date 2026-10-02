(*---
fsharp_space_before_class_constructor = true
fsharp_multiline_bracket_style = cramped
---*)
type ProtocolGlitchException =
    inherit CommunicationUnsuccessfulException

    new(message) = { inherit CommunicationUnsuccessfulException(message) }

    new(message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException(message, innerException) }
