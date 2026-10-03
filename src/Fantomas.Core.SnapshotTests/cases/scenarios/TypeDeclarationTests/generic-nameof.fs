(*---
fsharp_multiline_bracket_style = cramped
---*)
#r "nuget: FSharp.SystemTextJson"

open System.Text.Json
open System.Text.Json.Serialization
open System.Runtime.CompilerServices

nameof(+) // gives '+'
nameof op_Addition // gives 'op_Addition'

type C<'TType> =
    member _.TypeName = nameof<'TType> // Nameof with a generic type parameter via 'nameof<>'

/// Simplified version of EventStore's API
[<Struct; IsByRefLike>]
type RecordedEvent = { EventType: string; Data: ReadOnlySpan<byte> }

/// My concrete type:
type MyEvent =
    | AData of int
    | BData of string

// use 'nameof' instead of the string literal in the match expression
let deserialize (e: RecordedEvent) : MyEvent =
    match e.EventType with
    | nameof AData -> AData (JsonSerializer.Deserialize<int> e.Data)
    | nameof BData -> BData (JsonSerializer.Deserialize<string> e.Data)
    | t -> failwithf "Invalid EventType: %s" t
