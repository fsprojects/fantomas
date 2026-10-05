(*---
fsharp_max_record_width = 45
---*)
type BaseClass = class
    val string1 : string
    new(str) = { string1 = str }
    new() = { string1 = "" }
end

type DerivedClass =
    inherit BaseClass
    val string2 : string
    new (str1, str2) = { inherit BaseClass(str1); string2 = str2 }
    new (str2) = { inherit BaseClass(); string2 = str2 }