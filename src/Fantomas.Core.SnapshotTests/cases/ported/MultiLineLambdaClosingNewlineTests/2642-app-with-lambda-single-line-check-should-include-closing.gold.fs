module Foo =
    let part1 (lines: string seq) : int =
        lines
        |> Seq.map (fun s -> parse (s.AsSpan()))
        |> Seq.filter (fun (firstElf, secondElf) ->
            fullyContains firstElf secondElf || fullyContains secondElf firstElf
        )
        |> Seq.length
