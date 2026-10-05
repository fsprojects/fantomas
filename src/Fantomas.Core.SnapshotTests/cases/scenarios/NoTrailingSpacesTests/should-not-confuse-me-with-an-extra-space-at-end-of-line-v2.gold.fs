let ``should not extrude without positive distance`` () =
    let args =
        [| "-i"
           "input.dxf"
           "-o"
           "output.pdf"
           "--op"
           "extrude" |]

    (fun () -> parseCmdLine args |> ignore) |> should throw typeof<Argu.ArguParseException>
