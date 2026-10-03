let viewEntry todo dispatch =
    li [
        classList [
            ("completed", todo.completed)
            ("editing", todo.editing)
        ]
    ] [
        div [ ClassName "view" ] [
            input [
                ClassName "toggle"
                Type "checkbox"
                Checked todo.completed
                OnChange(fun _ -> Check(todo.id, (not todo.completed)) |> dispatch)
            ]
            label [
                OnDoubleClick(fun _ -> EditingEntry(todo.id, true) |> dispatch)
            ] [ str todo.description ]
            button [
                ClassName "destroy"
                OnClick(fun _ -> Delete todo.id |> dispatch)
            ] []
        ]
        input [
            ClassName "edit"
            valueOrDefault todo.description
            Name "title"
            Id("todo-" + (string todo.id))
            OnInput(fun ev ->
                UpdateEntry(todo.id, !!ev.target?value)
                |> dispatch)
            OnBlur(fun _ -> EditingEntry(todo.id, false) |> dispatch)
            onEnter (EditingEntry(todo.id, false)) dispatch
        ]
    ]
