Task.Run<CommandResult>(fun () ->
    // long
    // comment
    task)
|> ignore<Task<CommandResult>>

Task.Run<CommandResult>(task)
|> ignore<Task<CommandResult>>
