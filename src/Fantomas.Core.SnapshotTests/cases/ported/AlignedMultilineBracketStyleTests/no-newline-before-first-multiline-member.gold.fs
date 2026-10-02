type ShortExpressionInfo =
    {
        MaxWidth : int
        StartColumn : int
        ConfirmedMultiline : bool
    }
    member x.IsTooLong maxPageWidth currentColumn =
        currentColumn - x.StartColumn > x.MaxWidth // expression is not too long according to MaxWidth
        || (currentColumn > maxPageWidth) // expression at current position is not going over the page width

    member x.Foo() = ()
