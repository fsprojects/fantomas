(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
let expect =
    Result<Schema, SetError>.Ok { opts =
                                      [ Opts.anyOf
                                          ([ (Optional, Opt.flagTrue [ "first"; "f" ])
                                             (Optional, Opt.value [ "second"; "s" ]) ])
                                        Opts.oneOf
                                            (Optional,
                                             [ Opt.flag [ "third"; "f" ]
                                               Opt.valueWith "new value" [ "fourth"; "ssssssssssssssssssssssssssssssssssssssssssssssssssss" ] ]) ]
                                  args = []
                                  commands = [] }
