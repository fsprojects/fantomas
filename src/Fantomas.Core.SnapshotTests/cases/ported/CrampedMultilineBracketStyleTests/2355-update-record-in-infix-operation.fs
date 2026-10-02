(*---
fsharp_multiline_bracket_style = cramped
---*)
let rec meh () = ()
and seekReadParamExtras (retRes, paramsRes) (idx:int) =
    if seq = 0 then
        retRes := { !retRes with
                        //Marshal=(if hasMarshal then Some (fmReader (TaggedIndex(hfm_ParamDef, idx))) else None);
                        CustomAttrs = cas }
    else
        paramsRes.[seq - 1] <-
            { paramsRes.[seq - 1] with
                //Marshal=(if hasMarshal then Some (fmReader (TaggedIndex(hfm_ParamDef, idx))) else None)
                Default = (if hasDefault then USome (seekReadConstant (TaggedIndex(HasConstantTag.ParamDef, idx))) else UNone)
                Name = readStringHeapOption nameIdx
                Attributes = enum<ParameterAttributes> flags
                CustomAttrs = cas }
