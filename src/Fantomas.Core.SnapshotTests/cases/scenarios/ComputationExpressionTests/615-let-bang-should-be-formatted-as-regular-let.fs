let f =
  async {
    // Without binding newline after assignment sign preserved, which is expected behavior
    let r =
      match 0 with
      | _ -> ()

    return r
  }

let f2 =
  async {
    // When binding, newline force-removed, which makes the whole expression
    // on the right side to be indented.
    let! r = match 0 with
             | _ -> () |> async.Return

    return r
  }
