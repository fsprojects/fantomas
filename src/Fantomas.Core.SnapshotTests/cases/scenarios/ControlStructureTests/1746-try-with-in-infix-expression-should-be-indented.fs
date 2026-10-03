    let isAbstractNonVirtualMember (m: FSharpMemberOrFunctionOrValue) =
      // is an abstract member
      m.IsDispatchSlot
      // this member doesn't implement anything
      && (try m.ImplementedAbstractSignatures <> null &&  m.ImplementedAbstractSignatures.Count = 0 with _ -> true) // exceptions here trying to acces the member means we're safe
      // this member is not an override
      && not m.IsOverrideOrExplicitInterfaceImplementation
