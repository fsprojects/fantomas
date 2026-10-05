(*---
fsharp_max_value_binding_width = 120
---*)
type Text(text : string) =
    interface IDocument

    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
    