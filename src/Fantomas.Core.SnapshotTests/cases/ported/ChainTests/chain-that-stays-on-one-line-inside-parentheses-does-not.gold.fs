let b xs =
    xs
    |> (Seq.map (fun line ->
        transformTheLine line otherArgument finalArgument extraArgument))
