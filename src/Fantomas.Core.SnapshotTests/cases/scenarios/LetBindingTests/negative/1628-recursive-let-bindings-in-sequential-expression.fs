let foobar () =
    Console.WriteLine("Hello")

    let rec foo () = bar "Hello"
    and bar str = printf "%s" str |> ignore

    foo ()

let foobar () =
    let rec foo () = bar "Hello"
    and bar str = printf "%s" str |> ignore

    foo ()
