    type MyClassBase1() =
       let mutable z = 0
       abstract member Function1 : int -> int
       default u.Function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.Function1(a: int) = a + 1