let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    = {
    astContext with
        IsInsideMatchClausePattern = true
}
