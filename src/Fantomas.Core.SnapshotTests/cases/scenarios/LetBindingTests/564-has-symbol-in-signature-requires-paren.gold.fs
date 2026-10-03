module Bar =
    let foo(_: #(int seq)) = 1
    let meh(_: #seq<int>) = 2
    let evenMoreMeh(_: #seq<int>) : int = 2
