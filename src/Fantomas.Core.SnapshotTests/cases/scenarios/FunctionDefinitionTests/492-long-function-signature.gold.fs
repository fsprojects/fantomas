let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    let mutable jobDataMap = JobDataMap()
    jobDataMap.["task"] <- task

    let job =
        JobBuilder
            .Create<WrapperJob>()
            .UsingJobData(jobDataMap)
            .WithIdentity(taskName, groupName)
            .Build()

    1
