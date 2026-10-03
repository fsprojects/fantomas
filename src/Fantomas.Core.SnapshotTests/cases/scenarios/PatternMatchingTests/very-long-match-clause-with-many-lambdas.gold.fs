let MethInfoIsUnseen g m ty minfo =
    let isUnseenByObsoleteAttrib () =
        match
            BindMethInfoAttributes
                m
                minfo
                (fun ilAttribs -> Some foo)
                (fun fsAttribs -> Some bar)
                (fun provAttribs -> Some(CheckProvidedAttributesForUnseen provAttribs m))
                (fun _provAttribs -> None)
        with
        | Some res -> res
        | None -> false

    ()
