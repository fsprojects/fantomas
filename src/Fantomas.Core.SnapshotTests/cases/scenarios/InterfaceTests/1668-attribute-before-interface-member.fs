type internal WorkingShard =
    | Started of ReactoKinesixShardProcessor
    | Stopped of StoppedReason

and ReactoKinesixApp
    private
    (
        kinesis: IAmazonKinesis,
        dynamoDB: IAmazonDynamoDB,
        appName: string,
        streamName: string,
        workerId: string,
        processorFactory: IRecordProcessorFactory,
        config: ReactoKinesixConfig
    ) as this =


    interface IReactoKinesixApp with
        [<CLIEvent>] member this.OnInitialized = initializedEvent.Publish
        [<CLIEvent>] member this.OnBatchProcessed = batchProcessedEvent.Publish
