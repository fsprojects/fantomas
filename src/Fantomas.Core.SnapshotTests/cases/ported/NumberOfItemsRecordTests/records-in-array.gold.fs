let configurations =
    [| { Build = true
         Configuration = "RELEASE"
         Defines = [ "FOO" ] }
       { Build = true
         Configuration = "DEBUG"
         Defines = [ "FOO"; "BAR" ] } |]
