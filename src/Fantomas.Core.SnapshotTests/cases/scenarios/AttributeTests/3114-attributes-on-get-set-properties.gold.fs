[<Erase>]
type Object3D() =
    let mutable _position: Vector3 = null

    member this.position
        with [<Emit("$0.position")>] set v = _position <- v
        and [<Emit("$0.position = $1")>] get () = _position
