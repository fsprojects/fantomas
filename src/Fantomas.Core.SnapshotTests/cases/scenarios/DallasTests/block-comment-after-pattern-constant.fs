match tag with
            | 0 (* None *)  -> getInstancePropertyInfos (typ, [||], bindingFlags)
            | 1 (* Some *)  -> getInstancePropertyInfos (typ, [| "Value" |], bindingFlags)
            | _ -> failwith "fieldsPropsOfUnionCase"
