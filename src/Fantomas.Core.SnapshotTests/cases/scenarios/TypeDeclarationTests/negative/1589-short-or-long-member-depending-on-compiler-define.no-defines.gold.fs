type X =
    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon =
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> info.IsGenerated
        | _ -> false

    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased =
        x.IsMeasureableReprTycon
        #if !NO_EXTENSIONTYPING
        || x.IsProvidedErasedTycon
#endif
