let PublishValueDefn cenv env declKind (vspec: Val) =
    if
        (declKind = ModuleOrMemberBinding)
        && ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace)
        && (Option.isNone vspec.MemberInfo)
    then
        errorR (Error(FSComp.SR.tcNamespaceCannotContainValues (), vspec.Range))

    if
        (declKind = ExtrinsicExtensionBinding)
        && ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace)
    then
        errorR (Error(FSComp.SR.tcNamespaceCannotContainExtensionMembers (), vspec.Range))

    ()
