[<AbstractClass>]
type Shape2D(x0: float, y0: float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX
        with get () = x
        and set xval = x <- xval

    member this.CenterY
        with get () = y
        and set yval = y <- yval

    abstract Area: float with get
    abstract Perimeter: float with get
    abstract Name: string with get

    member this.Move dx dy =
        x <- x + dx
        y <- y + dy

    abstract member Rotate: float -> unit
    default this.Rotate(angle) = rotAngle <- rotAngle + angle
