let expect =
    Result<Schema, SetError>.Ok
        { opts =
            [ Opts.anyOf (
                  [ (Optional, Opt.flagTrue [ "first"; "f" ])
                    (Optional, Opt.value [ "second"; "s" ]) ]
              )
              Opts.oneOf (
                  Optional,
                  [ Opt.flag [ "third"; "f" ]
                    Opt.valueWith
                        "new value"
                        [ "fourth"
                          "ssssssssssssssssssssssssssssssssssssssssssssssssssss" ] ]
              ) ]
          args = []
          commands = [] }
