let r =
    {
        a = x
        b = y
        z = c
    }

let s = { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

let r' =
    { r with
        a = x
        b = y
        z = c
    }

let s' = { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f
    r
    {
        a = x
        b = y
        z = c
    }

g s { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f
    r'
    { r with
        a = x
        b = y
        z = c
    }

g s' { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }
