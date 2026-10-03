let rec loop () =
  async {
    let! msg = inbox.Receive ()

    match msg with
    | Handle (eventSource, command, reply) ->
      let! stream = eventSource |> eventStore.GetStream

      let newEvents =
        stream
        |> Result.map (
          asEvents
          >> behaviour command
          >> enveloped eventSource
        )

      let! result =
        newEvents
        |> function
          | Ok events -> eventStore.Append events
          | Error err -> async {return Error err}

      do reply.Reply result

      return! loop ()
  }
