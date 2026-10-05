(*---
fsharp_multiline_bracket_style = cramped
---*)
    let implementer() =
        { new ISecond with
            member this.H() = ()
            member this.J() = ()
          interface IFirst with
            member this.F() = ()
            member this.G() = () }