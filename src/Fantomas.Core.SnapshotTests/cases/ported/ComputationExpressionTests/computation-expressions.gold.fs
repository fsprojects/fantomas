let comp =
    eventually {
        for x in 1..2 do
            printfn " x = %d" x

        return 3 + 4
    }
