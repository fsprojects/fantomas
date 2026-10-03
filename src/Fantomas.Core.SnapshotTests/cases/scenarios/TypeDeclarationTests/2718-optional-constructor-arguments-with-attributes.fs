type CsvFile
    private
    (
        readerFunc: Func<TextReader>,
        [<Optional>] ?separators,
        [<Optional>] ?quote,
        [<Optional>] ?hasHeaders,
        [<Optional>] ?ignoreErrors,
        [<Optional>] ?skipRows
    ) as this =
    inherit CsvFile<CsvRow>
        (
            Func<_, _, _>(fun this columns -> CsvRow(this :?> CsvFile, columns)),
            Func<_, _>(fun row -> row.Columns),
            readerFunc,
            defaultArg separators "",
            defaultArg quote '"',
            defaultArg hasHeaders true,
            defaultArg ignoreErrors false,
            defaultArg skipRows 0
        )
