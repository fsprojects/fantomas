(*---
fsharp_max_record_width = 50
---*)
[<Test>]
let ``classes and private implicit constructors`` () =
    formatSourceString false """
    type MyClass2 private (dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data""" { config with
                                                                 MaxFunctionBindingWidth = 120 }
