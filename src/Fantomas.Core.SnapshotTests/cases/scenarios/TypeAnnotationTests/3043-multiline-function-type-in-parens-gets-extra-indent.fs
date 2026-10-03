type MyCustomTypeWithAPrettyLongDescribingName = MyCustomConstructor1

let private myFunction
    : string
          -> (MyCustomTypeWithAPrettyLongDescribingName -> MyCustomTypeWithAPrettyLongDescribingName -> MyCustomTypeWithAPrettyLongDescribingName)
          -> unit =
    fun a fn -> ()
