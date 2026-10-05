type A() =
    member this.X with get():int = 1
    member this.Y with get():int = 1 and set (_:int):unit = ()
    member this.Z with set (_:int):unit = () and get():int = 1
