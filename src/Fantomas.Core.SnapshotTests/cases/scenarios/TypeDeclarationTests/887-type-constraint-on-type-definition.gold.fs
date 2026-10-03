type OuterType =
    abstract Apply<'r> : InnerType<'r> -> 'r when 'r : comparison
