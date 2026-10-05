(*---
fsharp_max_infix_operator_expression = 50
---*)
Task.Run<CommandResult>(fun () ->
    // long
    // comment
    task)
|> ignore<Task<CommandResult>>

Task.Run<CommandResult> (task)
|> ignore<Task<CommandResult>>
