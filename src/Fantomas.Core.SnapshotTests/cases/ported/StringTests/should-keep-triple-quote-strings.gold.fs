[<EntryPoint>]
let main argv =
    use fun1 =
        R.eval (
            R.parse (
                text =
                    """
    function(i) {
        x <- rnorm(1000)
        y <- rnorm(1000)
        m <- lm(y~x)
        m$coefficients[[2]]
    }
    """
            )
        )

    0
