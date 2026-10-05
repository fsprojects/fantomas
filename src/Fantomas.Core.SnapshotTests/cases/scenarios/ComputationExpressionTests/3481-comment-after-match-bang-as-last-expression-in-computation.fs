let f () = task {
    match! g () with
    | Ok _ -> ()
    | Error _ -> ()

    // comment
}

let x = 1
