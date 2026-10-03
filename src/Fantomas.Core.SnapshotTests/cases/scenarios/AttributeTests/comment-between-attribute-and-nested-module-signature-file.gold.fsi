[<AutoOpen>]
// Having members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
module AsyncOptionCEExtensions =

    type AsyncOptionBuilder with
        member inline Source: string -> string
