(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
let foo () =
    if someCondition then
        0
    else
    let config = Configuration.Read "/myfolder/myfile.xml"
    let result = Process.main config otherArg
    if result.IsOk then
        0
    else
        -1
