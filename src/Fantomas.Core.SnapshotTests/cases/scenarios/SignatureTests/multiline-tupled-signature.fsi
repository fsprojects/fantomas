(*---
max_line_length = 60
---*)
    namespace Oslo
    type Meh =
        member ResolveDependencies:
            scriptDirectory: string
            * scriptName: string
            * scriptExt: string
            * timeout: int ->
            obj
