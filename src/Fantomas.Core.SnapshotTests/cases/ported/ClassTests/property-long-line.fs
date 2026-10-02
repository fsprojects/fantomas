type T() =
    member __.Property = "hello"
let longNamedFunlongNamedFunlongNamedFunlongNamedFunlongNamedFun (x:T) = x
let longNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClass = T()

System.String.Concat("a", "b" +
                            longNamedFunlongNamedFunlongNamedFunlongNamedFunlongNamedFun(longNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClass).Property)
