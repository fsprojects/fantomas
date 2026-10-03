(*---
max_line_length = 80
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        { new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable }
