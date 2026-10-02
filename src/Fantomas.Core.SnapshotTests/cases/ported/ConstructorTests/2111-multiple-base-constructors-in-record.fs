(*---
max_line_length = 100
fsharp_multiline_bracket_style = cramped
---*)
type UnhandledWebException =
    inherit Exception

    new(status: WebExceptionStatus, innerException: Exception) =
        { inherit Exception(SPrintF1
                                "Backend not prepared for this WebException with Status[%i]"
                                (int status),
                            innerException) }

    new(info: SerializationInfo, context: StreamingContext) =
        { inherit Exception(info, context) }
