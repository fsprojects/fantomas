(*---
max_line_length = 80
fsharp_multi_line_lambda_closing_newline = true
---*)
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun { Path = path; Key = key; Value = value; Attempt = attempt } -> metadata.Add(key, value))
        .Create()
