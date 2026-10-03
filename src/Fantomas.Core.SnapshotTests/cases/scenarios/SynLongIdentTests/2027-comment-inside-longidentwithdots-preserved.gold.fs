let path =
    match normalizedPath with
    | path ->
        path // translate path to Python relative syntax
            .Replace("../../../", "....")
