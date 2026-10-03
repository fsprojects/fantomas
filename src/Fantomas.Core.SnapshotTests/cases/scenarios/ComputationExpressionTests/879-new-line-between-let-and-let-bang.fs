(*---
indent_size = 2
fsharp_space_before_uppercase_invocation = true
fsharp_space_around_delimiter = false
fsharp_max_infix_operator_expression = 50
---*)
let rec loop () =
        async {
          let! msg = inbox.Receive()

          match msg with
          | Handle (eventSource,command,reply) ->
              let! stream = eventSource |> eventStore.GetStream

              let newEvents =
                stream |> Result.map (asEvents >> behaviour command >> enveloped eventSource)

              let! result =
                newEvents
                |> function
                    | Ok events -> eventStore.Append events
                    | Error err -> async { return Error err }

              do reply.Reply result

              return! loop ()
        }
