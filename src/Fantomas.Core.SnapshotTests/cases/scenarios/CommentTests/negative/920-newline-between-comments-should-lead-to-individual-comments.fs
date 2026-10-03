[<AllowNullLiteral>]
type IExports =
    abstract DataSet: DataSetStatic
    abstract DataView: DataViewStatic
    abstract Graph2d: Graph2dStatic
    abstract Timeline: TimelineStatic
    // abstract Timeline: TimelineStaticStatic
    abstract Network: NetworkStatic

// type [<AllowNullLiteral>] MomentConstructor1 =
//     [<Emit "$0($1...)">] abstract Invoke: ?inp: MomentInput * ?format: MomentFormatSpecification * ?strict: bool -> Moment

// type [<AllowNullLiteral>] MomentConstructor2 =
//     [<Emit "$0($1...)">] abstract Invoke: ?inp: MomentInput * ?format: MomentFormatSpecification * ?language: string * ?strict: bool -> Moment

// type MomentConstructor =
//     U2<MomentConstructor1, MomentConstructor2>
