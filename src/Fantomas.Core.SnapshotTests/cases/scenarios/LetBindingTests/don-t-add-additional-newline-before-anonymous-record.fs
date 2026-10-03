(*---
fsharp_multiline_bracket_style = cramped
---*)
let useOverviewPerMonth () =
    let { Events = events } = useModel ()

    let months =
        events
        |> List.choose (fun msg ->
            match msg with
            | Event.AddIncome({ Created = created })
            | Event.AddExpense({ Created = created }) -> Some(created.Month, created.Year)
            | _ -> None)
        |> List.distinct
        |> List.sort
        |> List.groupBy snd
        |> List.map (fun (year, months) ->
            let rows =
                months
                |> List.map (fun (m, y) ->
                    {| name = getMonthName m
                       month = m
                       balance = Projections.calculateBalance m y events |})
                |> List.toArray

            let balance =
                rows |> Array.sumBy (fun mth -> mth.balance)

            {| name = year
               months = rows
               balance = balance |})
        |> List.toArray

    months
