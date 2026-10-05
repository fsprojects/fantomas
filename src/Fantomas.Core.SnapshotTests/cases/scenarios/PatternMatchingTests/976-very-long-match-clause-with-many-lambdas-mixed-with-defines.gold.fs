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
                (fun _provAttribs -> None)
#endif
        with
        | Some res -> res
        | None -> false

    ()
