{| payload with reason = reason |}
|> Json.serialize<{|
    reason: string
    old: bool
    ``new``: bool
|}>
