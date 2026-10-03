    // Check for the [<ProjectionParameter>] attribute on an argument position
    let isCustomOperationProjectionParameter i (nm: Ident) =
        match tryGetArgInfosForCustomOperator nm with
        | None -> false
        | Some argInfosForOverloads ->
            let vs =
                argInfosForOverloads |> List.map (function
                    | None -> false
                    | Some argInfos ->
                        i < argInfos.Length &&
                        let (_, argInfo) = List.item i argInfos
                        HasFSharpAttribute cenv.g cenv.g.attrib_ProjectionParameterAttribute argInfo.Attribs)
            if List.allEqual vs then vs.[0]
            else
                let opDatas = (tryGetDataForCustomOperation nm).Value
                let (opName, _, _, _, _, _, _, _j, _) = opDatas.[0]
                errorR(Error(FSComp.SR.tcCustomOperationInvalid opName, nm.idRange))
                false
