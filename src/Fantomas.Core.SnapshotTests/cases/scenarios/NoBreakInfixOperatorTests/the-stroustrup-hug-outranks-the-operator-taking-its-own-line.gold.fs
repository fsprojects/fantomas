let long =
    xs
        .ReplaceEverythingEverywhere(11111, 22222)
        .ReplaceEverythingEverywhere(33333, 44444)
        .TrimEnd() = [
        "aaaaaaaaaaaaaaaaaaaaaa"
        "bbbbbbbbbbbbbbbbbbbbbb"
        "cccccccccccccccccccccc"
        "dddddddddddddddddddddd"
    ]
