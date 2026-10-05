let inline test< ^foo> (foo: ^foo) =
    let bar = typeof< ^foo>
    bar.Name
