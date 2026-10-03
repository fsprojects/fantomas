open Xunit
open FSharp.Core
open Swensen.Unquote

type FormattingSpecs() =

    [<Fact>]
    let ``true is true``() = test <@ true = true @>

    [<Fact>]
    let ``false is false``() = test <@ false = false @>
