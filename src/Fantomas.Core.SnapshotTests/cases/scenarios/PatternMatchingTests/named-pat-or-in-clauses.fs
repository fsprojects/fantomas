let (|MFMember|MFStaticMember|MFConstructor|MFOverride|) (mf: MemberFlags) =
    match mf.MemberKind with
    | MemberKind.ClassConstructor
    | MemberKind.Constructor -> MFConstructor()
    | MemberKind.Member
    | MemberKind.PropertyGet
    | MemberKind.PropertySet
    | MemberKind.PropertyGetSet as mk ->
        if mf.IsInstance && mf.IsOverrideOrExplicitImpl
        then MFOverride mk
        elif mf.IsInstance
        then MFMember mk
        else MFStaticMember mk
