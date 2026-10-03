(st :> IProvidedCustomAttributeProvider)
    .GetHasTypeProviderEditorHideMethodsAttribute(
        info.ProvidedType.TypeProvider
            .PUntaintNoFailure(id)
    )
