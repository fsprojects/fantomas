type Object3D() =
    [<X>]
    member this.position
        with [<Y>] set v = _position <- v 
        and [<Z>] get () = _position
