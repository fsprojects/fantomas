let test (dict: System.Collections.Generic.IDictionary<string, unit -> unit -> unit>) =
    let key = "foo"
    dict[key] () ()
