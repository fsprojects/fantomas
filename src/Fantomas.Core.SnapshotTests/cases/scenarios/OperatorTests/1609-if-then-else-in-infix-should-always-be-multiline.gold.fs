module Foo =
    let bar () =

        if
            not
            <| RuntimeInformation.IsOSPlatform OSPlatform.Windows
        then
            raise
            <| PlatformNotSupportedException (
                "Blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"
            )

        lazy
            (let foo = bar

             if ret then "" else ""
             |> log.LogInformation

             ret)
