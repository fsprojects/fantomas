(*---
fsharp_max_value_binding_width = 50
fsharp_max_function_binding_width = 50
---*)
    let notFound () =
        let json = Encode.string "Not found" |> Encode.toString 4

        new HttpResponseMessage(HttpStatusCode.NotFound,
                                Content = new StringContent(json, System.Text.Encoding.UTF8, "application/json"))
