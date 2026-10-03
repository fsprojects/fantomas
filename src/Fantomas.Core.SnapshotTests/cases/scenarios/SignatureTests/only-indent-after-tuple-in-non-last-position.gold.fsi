namespace Oslo

type Meh =
    member ResolveDependencies:
        criptName: string ->
        foo: string ->
        scriptDirectory:
            string *
        scriptName: string -> // after a tuple, mixed needs an indent
            scriptName: string ->
                obj
