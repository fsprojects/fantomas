module internal Fantomas.Core.MCPEvents

open System
open System.Collections.Concurrent
open Fantomas.FCS.Parse
open SyntaxOak

[<NoComparison; NoEquality>]
type EventKind =
    | SourceTextCreated
    | ParsedBaseUntypedTree of
        baseTree: Fantomas.FCS.Syntax.ParsedInput *
        baseDiagnostics: Fantomas.FCS.Parse.FSharpParserDiagnostic list
    | SourceCodeHadDefines of combinations: Fantomas.Core.DefineCombination list
    | ParsedUntypedTreeWithDefines of
        tree: Fantomas.FCS.Syntax.ParsedInput *
        diagnostics: Fantomas.FCS.Parse.FSharpParserDiagnostic list *
        defineCombination: Fantomas.Core.DefineCombination
    | ParseFailed of diagnostics: FSharpParserDiagnostic list
    | FoundTrivia of TriviaNode array
    | CreatedOakRaw of oak: Oak
    | CreatedOak of string
    | CollectedEventsAfterCodePrinter of string
    | UnExpectedExceptionHappened of exn

[<NoComparison; NoEquality>]
type MCPEvent = { Created: DateTime; Kind: EventKind }

let capturedEvents = ConcurrentQueue<MCPEvent>()

let addEvent eventKind =
    capturedEvents.Enqueue(
        { Kind = eventKind
          Created = DateTime.Now }
    )

let resetEvents () =
    while capturedEvents.TryDequeue() |> fst do
        ()
