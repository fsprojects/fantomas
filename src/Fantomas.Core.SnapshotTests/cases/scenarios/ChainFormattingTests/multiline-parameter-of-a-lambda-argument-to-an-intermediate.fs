(*---
max_line_length = 80
---*)
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun { Path = path; Key = key; Value = value; Attempt = attempt } -> metadata.Add(key, value))
        .Create()
