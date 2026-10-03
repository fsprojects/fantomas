(*---
fsharp_max_function_binding_width = 120
---*)
    type MyClass2(dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data