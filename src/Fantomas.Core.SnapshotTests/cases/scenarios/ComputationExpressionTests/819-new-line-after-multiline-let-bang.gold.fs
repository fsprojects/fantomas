let x data =
    async {
        let! bar =
            data
            |> Array.map id
            |> Array.filter ((=) 1)
            |> Array.countBy id
            |> async.Return

        return bar
    }

let y data =
    async {
        let bar =
            data
            |> Array.map id
            |> Array.filter ((=) 1)
            |> Array.countBy id
            |> async.Return

        return bar
    }

let z =
    let bar =
        data
        |> Array.map id
        |> Array.filter ((=) 1)
        |> Array.countBy id

    bar
