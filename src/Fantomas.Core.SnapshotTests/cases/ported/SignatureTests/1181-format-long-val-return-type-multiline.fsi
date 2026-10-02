(*---
fsharp_space_before_colon = true
---*)
namespace TypeEquality

[<RequireQualifiedAccess>]
module Teq =

    [<RequireQualifiedAccess>]
    module Cong =

        val domainOf<'domain1, 'domain2, 'range1, 'range2> : Teq<'domain1 -> 'range1, 'domain2 -> 'range2>
             -> Teq<'domain1, 'domain2>
