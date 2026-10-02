type V = {
    X: SomeFieldType
    Y: OhSomethingElse
    Z: ALongTypeName
} with
    member this.Coordinate = (this.X, this.Y, this.Z)
