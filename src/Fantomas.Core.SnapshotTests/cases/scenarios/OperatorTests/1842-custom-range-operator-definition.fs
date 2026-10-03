(*---
fsharp_max_infix_operator_expression = 50
---*)
[<AutoOpen>] //making auto open allows us not to have to fully qualify module properties
module DecompilationTests
open Xunit
open Swensen.Unquote

module TopLevelOpIsolation3 =
    let (..) x y z = Seq.singleton (x + y + z)
    [<Fact>]
    let ``issue 91: op_Range first class syntax for seq return type but arg mismatch`` () =
        <@ (..) 1 2 3 @> |> decompile =! "TopLevelOpIsolation3.(..) 1 2 3"

    let (.. ..) x y z h = Seq.singleton (x + y + z + h)
    [<Fact>]
    let ``issue 91: op_RangeStep first class syntax for seq return type but arg mismatch`` () =
        <@ (.. ..) 1 2 3 4 @> |> decompile =! "TopLevelOpIsolation3.(.. ..) 1 2 3 4"
