match item.Item with
| :? FSharpToolTipText as titem -> ()
| :? (string * XmlDoc) as tip -> ()
| _ -> ()
