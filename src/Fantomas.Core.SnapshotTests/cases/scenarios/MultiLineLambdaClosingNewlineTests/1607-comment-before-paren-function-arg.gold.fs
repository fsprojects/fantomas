namespace Bar

[<RequireQualifiedAccess>]
module Foo =
    /// Blah
    let bang<'a when 'a : equality> (a : Foo<'a>) (ans : ('a * System.TimeSpan) list) : bool =
        List.length x = List.length y
        && List.forall2
            //
            (fun (a, ta) (b, tb) -> a.Equals b && ta = tb)
            x
            y
