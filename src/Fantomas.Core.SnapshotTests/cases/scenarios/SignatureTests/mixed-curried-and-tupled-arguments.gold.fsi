namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory:
            string *
        scriptName: string ->
            scriptName: string ->
                obj
