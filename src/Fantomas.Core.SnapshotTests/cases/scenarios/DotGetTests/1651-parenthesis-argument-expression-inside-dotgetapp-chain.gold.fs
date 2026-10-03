module Foo =
    let bar () =
        let saveDir =
            fs.DirectoryInfo
                .FromDirectoryName(
                    fs.Path.Combine ((ThingThing.rootRoot fs thingThing).FullName, "tada!")
                )
                .EnumerateDirectories()
            |> Seq.exactlyOne

        ()
