(*---
fsharp_space_before_class_constructor = true
---*)
type IInterface =
    interface
    end

type IInterface2 =
    interface
        abstract member X: unit -> unit
    end

type IInterface3 =
    abstract member X: unit -> unit
