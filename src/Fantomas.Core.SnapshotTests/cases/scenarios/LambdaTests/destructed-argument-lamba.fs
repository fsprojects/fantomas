List.filter (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
