[<Test>]
let ``defines inside string, escaped quote`` () =
    let source = "
let a = \"\\\"
#if FOO
  #if BAR
  #endif
#endif
\"
"

    getDefines source == []
