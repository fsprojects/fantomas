module Fantomas.Suggestion

// Answering "what did they probably mean" is not about any one kind of name: an `.editorconfig`
// setting and a command line flag are both something someone typed and misspelled. The two lists
// to compare against are the callers' business; the comparison is this module's.

let editDistance (limit: int) (left: string) (right: string) : int =
    // The cap answers without measuring when the two lengths already differ by more than it.
    // Anything past that is measured in full, so two names of the same length are compared
    // character by character however unalike they are.
    if abs (left.Length - right.Length) > limit then
        limit + 1
    else

    let mutable previous: int array = Array.init (right.Length + 1) id
    let mutable current: int array = Array.zeroCreate<int>(right.Length + 1)

    for row in 1 .. left.Length do
        current[0] <- row

        for column in 1 .. right.Length do
            let substitution: int =
                previous[column - 1] + (if left[row - 1] = right[column - 1] then 0 else 1)

            current[column] <- min (min (current[column - 1] + 1) (previous[column] + 1)) substitution

        let swap: int array = previous
        previous <- current
        current <- swap

    min previous[right.Length] (limit + 1)

let nearest (limit: int) (candidates: string list) (term: string) : string option =
    candidates
    |> List.choose (fun (candidate: string) ->
        match editDistance limit term candidate with
        | distance when distance <= limit -> Some(candidate, distance)
        | _ -> None
    )
    |> List.sortBy snd
    |> List.tryHead
    |> Option.map fst
