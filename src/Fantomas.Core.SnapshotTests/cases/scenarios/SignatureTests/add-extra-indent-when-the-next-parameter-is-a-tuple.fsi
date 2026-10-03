(*---
max_line_length = 12
---*)
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory: string * scriptName: string ->
        scriptName: string
        * scriptExt: string
        * timeout: int ->
        obj
