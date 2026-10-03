Seq.takeWhile (function
    | Write ""
    // for example:
    // type Foo =
    //     static member Bar () = ...
    | IndentBy _
    | WriteLine
    | SetAtColumn _
    | Write " -> "
    | CommentOrDefineEvent _ -> true
    | _ -> false)
