type TypeDefnUnionNode
    (typeNameNode, accessibility: SingleTextNode option, unionCases: UnionCaseNode list, members: MemberDefn list, range)

    =
    inherit NodeBase(range)
