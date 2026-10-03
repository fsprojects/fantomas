let rec make item depth =
    if depth > 0 then
        Tree(
            { Left = make (2 * item - 1) (depth - 1)
              Right = make (2 * item) (depth - 1) },
            item
        )
    else
        Tree(defaultof<_>, item)
