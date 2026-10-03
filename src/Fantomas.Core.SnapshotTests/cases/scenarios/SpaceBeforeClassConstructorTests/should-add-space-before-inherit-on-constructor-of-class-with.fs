(*---
fsharp_space_before_class_constructor = true
---*)
type DerivedClass =
    inherit BaseClass

    val string2: string

    new(str1, str2) =
        { inherit BaseClass(str1)
          string2 = str2 }
