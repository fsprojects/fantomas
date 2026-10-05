module Foo =
    let bar =
        if
            Regex("long long long long long long long long long")
                .Match(s)
                .Success
        then
            None
        else
            Some "hi"
