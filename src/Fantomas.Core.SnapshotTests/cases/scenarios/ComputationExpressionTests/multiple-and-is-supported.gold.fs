// Reads the values of x, y and z concurrently, then applies f to them
``parallel`` {
    let! x = slowRequestX ()
    and! y = slowRequestY ()
    and! z = slowRequestZ ()
    return f x y z
}
