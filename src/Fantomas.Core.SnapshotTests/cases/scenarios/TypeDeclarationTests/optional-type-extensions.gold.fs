/// Define a new member method FromString on the type Int32.
type System.Int32 with
    member this.FromString(s: string) = System.Int32.Parse(s)
