let retrySql<'a> =
    Policy
        .HandleTransientSqlError()
        .WaitAndRetryAsync(
            List.map TimeSpan.FromSeconds [ 1.; 2.; 3. ],
            fun ex ts i ctx ->
                Log.Information(
                    ex,
                    "DB retry policy: Exception thrown, performing retry {RetryNo}, operation {OperationKey}",
                    i,
                    ctx.OperationKey
                )
        )
        .AsAsyncPolicy<'a>()
