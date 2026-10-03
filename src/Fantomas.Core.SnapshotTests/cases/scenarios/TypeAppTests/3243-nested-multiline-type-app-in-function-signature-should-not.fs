let makePipeline''
    (mode: ExecutionMode<Marker>)
    (resolver: IResolver option Marker)
    (args: Arguments Marker)
    (logging: Marker<ExtraLogging>)
    (processing:
        (PipelineStep<
            Map<string,
                ((Table * IStepMetadata)
                 * Operation seq
                 * FileInfo ResultKind seq
                 * (NodeInfo<string, string> * DayLogs) seq)>,
            PipelineCrate>)
            ->
        (Map<string,
            ((Table * IStepMetadata)
             * Operation seq
             * FileInfo ResultKind seq
             * (NodeInfo<string, string> * DayLogs) seq)> Marker)
            ->
        PipelineStep<unit>)
    =
    ()
