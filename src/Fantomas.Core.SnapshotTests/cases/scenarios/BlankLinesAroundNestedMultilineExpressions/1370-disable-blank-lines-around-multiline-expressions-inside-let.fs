(*---
fsharp_blank_lines_around_nested_multiline_expressions = false
---*)
let emit nodes =
    let an = AssemblyName("a")
    let ab =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            an,
            AssemblyBuilderAccess.RunAndCollect
        )
    let mb = ab.DefineDynamicModule("a")
    let tb = mb.DefineType("Program")
    nodes
