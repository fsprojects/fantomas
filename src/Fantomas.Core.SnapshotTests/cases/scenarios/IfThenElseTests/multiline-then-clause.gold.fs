[<EntryPoint>]
let main argv =
    let fileToFile (inFile: string) (outFile: string) =
        try
            use buffer =
                if hasByteOrderMark then
                    new StreamWriter(
                        new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                        Encoding.UTF8
                    )
                else
                    new StreamWriter(outFile)

            buffer.Flush()
        with exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn

    0
