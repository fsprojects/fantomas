module PoorlyIndented =

    let findThing dependency thingId =
        use cmd =
            query
                SomeDatabase.CreateCommand<"
                       select name
                       from things
                       where id = :id
      "          >
                dependency

        cmd.AsyncExecute(id = thingId)
