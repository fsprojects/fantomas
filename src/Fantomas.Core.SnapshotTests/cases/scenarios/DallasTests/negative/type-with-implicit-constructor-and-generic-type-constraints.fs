[<System.Diagnostics.DebuggerDisplay "Count = {Count}">]
[<Sealed>]
type internal Set<'T, 'ComparerTag> when 'ComparerTag :> IComparer<'T>(comparer: IComparer<'T>, tree: SetTree<'T>) =

    static let refresh (s: Set<_, _>) t =
        Set<_, _>(comparer = s.Comparer, tree = t)
