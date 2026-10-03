let goThroughFsharpTicketsAsync () =
    task {
        let mutable ticketNumber = 1

        while! doesTicketExistAsync ticketNumber do
            printfn $"Found a PR or issue #{ticketNumber}."
            ticketNumber <- ticketNumber + 1

        printfn $"#{ticketNumber} is not created yet."
    }
