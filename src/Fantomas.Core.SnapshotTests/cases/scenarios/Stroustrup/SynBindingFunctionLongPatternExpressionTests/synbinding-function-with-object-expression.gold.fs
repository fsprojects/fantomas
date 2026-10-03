let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    = {
    new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable
}
