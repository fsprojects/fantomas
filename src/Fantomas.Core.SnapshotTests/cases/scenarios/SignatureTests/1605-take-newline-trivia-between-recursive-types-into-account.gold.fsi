namespace Test

///
type Foo =
    ///
    | Bar

///
and internal Hi<'a> =
    ///
    abstract Apply<'b> : Foo -> 'b


///
and [<CustomEquality>] Bang =
    internal
        {
            LongNameBarBarBarBarBarBarBar: int
        }
    ///
    override GetHashCode: unit -> int
