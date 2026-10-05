let MethInfoIsUnseen g m ty minfo =
    let isUnseenByObsoleteAttrib () =
        match
            BindMethInfoAttributes
                m
                minfo
                (fun ilAttribs -> Some foo)
                (fun fsAttribs -> Some bar)
                #if !NO_EXTENSIONTYPING
                (fun provAttribs -> Some(CheckProvidedAttributesForUnseen provAttribs m))
        #else
        #endif
        with
        | Some res -> res
        | None -> false

    ()
