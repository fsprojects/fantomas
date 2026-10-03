match x with
| SynMemberDefn.ImplicitCtor(_, attrs, ctorArgs, _, _xmlDoc, range) -> [
    yield mkNode SynMemberDefn_ImplicitCtor range
    yield! (visitSynAttributeLists attrs)
    yield! visitSynSimplePats ctorArgs
  ]
| SynMemberDefn.ImplicitInherit(inheritType, inheritArgs, _, range) -> [
    yield mkNode SynMemberDefn_ImplicitInherit range
    yield! visitSynType inheritType
    yield! visitSynExpr inheritArgs
  ]
