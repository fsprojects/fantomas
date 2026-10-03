/// Specifies the formatting behaviour of JSON values
[<RequireQualifiedAccess>]
type JsonSaveOptions =
    | None = 0

    /// Print the JsonValue in one line in a compact way
    | DisableFormatting = 1

    | OtherFormatting = 2
