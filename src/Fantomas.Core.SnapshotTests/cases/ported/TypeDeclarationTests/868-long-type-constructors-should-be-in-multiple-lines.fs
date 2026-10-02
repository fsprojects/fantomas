(*---
max_line_length = 55
fsharp_space_before_colon = true
---*)
type VersionMismatchDuringDeserializationException(message: string, innerException: System.Exception) =
    inherit System.Exception(message, innerException)
