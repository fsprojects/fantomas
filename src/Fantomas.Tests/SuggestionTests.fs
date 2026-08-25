module Fantomas.Tests.SuggestionTests

open NUnit.Framework
open FsUnitTyped
open Fantomas

let private settings: string list =
    [ "max_line_length"; "indent_size"; "end_of_line" ]

[<Test>]
let ``a string is no edits from itself`` () =
    Suggestion.editDistance 2 "check" "check" |> shouldEqual 0

[<Test>]
let ``a dropped letter is one edit`` () =
    Suggestion.editDistance 2 "chek" "check" |> shouldEqual 1

[<Test>]
let ``a swapped pair is two edits`` () =
    Suggestion.editDistance 3 "cehck" "check" |> shouldEqual 2

[<Test>]
let ``the answer stops at the limit rather than being measured in full`` () =
    // The cap is what lets a long list be scanned per formatted file. Anything above the limit
    // reports as one past it rather than as its real distance.
    Suggestion.editDistance 2 "check" "something else entirely" |> shouldEqual 3

[<Test>]
let ``the nearest candidate within the limit is named`` () =
    Suggestion.nearest 2 settings "max_line_lenght"
    |> shouldEqual (Some "max_line_length")

[<Test>]
let ``nothing is named when the nearest is too far away`` () =
    // Past the limit a guess is worse than silence, which is what keeps a message from inventing
    // a setting the reader never meant.
    Suggestion.nearest 2 settings "some_other_tools_key" |> shouldEqual None

[<Test>]
let ``an empty candidate list names nothing`` () =
    Suggestion.nearest 2 [] "max_line_length" |> shouldEqual None

[<Test>]
let ``a tie is settled by the order of the candidates`` () =
    // Stable, so the caller's ordering decides. Nothing reaches this by mistyping; it takes a
    // string built on purpose to sit between two candidates.
    Suggestion.nearest 2 [ "abc"; "abd" ] "abe" |> shouldEqual (Some "abc")
