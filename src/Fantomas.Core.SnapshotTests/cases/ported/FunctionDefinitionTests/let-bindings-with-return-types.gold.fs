let divide x y =
    let stream: System.IO.FileStream = System.IO.File.Create("test.txt")
    let writer: System.IO.StreamWriter = new System.IO.StreamWriter(stream)

    try
        writer.WriteLine("test1")
        Some(x / y)
    finally
        writer.Flush()
        printfn "Closing stream"
        stream.Close()
