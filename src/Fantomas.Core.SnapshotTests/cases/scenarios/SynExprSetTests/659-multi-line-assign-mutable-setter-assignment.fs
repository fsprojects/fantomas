(*---
max_line_length = 80
fsharp_max_infix_operator_expression = 50
---*)
ctx.Response.Headers.[HeaderNames.ContentType] <- Constants.jsonApiMediaType
                                                  |> StringValues
ctx.Response.Headers.[HeaderNames.ContentLength] <- bytes.Length
                                                    |> string
                                                    |> StringValues
ctx.Response.SomeElseThatIsMutable <- [ "a"; "b"; "c" ]
                                      |> List.indexed
                                      |> List.map snd
