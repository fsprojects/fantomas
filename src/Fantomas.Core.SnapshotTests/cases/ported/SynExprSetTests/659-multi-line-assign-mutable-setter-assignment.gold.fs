ctx.Response.Headers.[HeaderNames.ContentType] <-
    Constants.jsonApiMediaType |> StringValues

ctx.Response.Headers.[HeaderNames.ContentLength] <-
    bytes.Length |> string |> StringValues

ctx.Response.SomeElseThatIsMutable <-
    [ "a"; "b"; "c" ] |> List.indexed |> List.map snd
