(*---
max_line_length = 50
---*)
let job =
    JobBuilder
        .UsingJobData(jobDataMap)
        .Create<WrapperJob>()
        .WithIdentity(taskName, groupName)
        .Build()
