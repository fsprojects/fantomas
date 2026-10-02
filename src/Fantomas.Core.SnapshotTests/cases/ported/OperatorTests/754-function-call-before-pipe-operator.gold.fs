[<Test>]
let ``attribute on module after namespace`` () =
    formatSourceString
        false
        """namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
"""
