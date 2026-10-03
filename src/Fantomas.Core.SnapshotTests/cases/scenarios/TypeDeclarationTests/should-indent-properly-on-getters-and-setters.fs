type A() =
    override this.Address with set v =
        let x =
             match _kbytes.GetAddress(8) with
             | Some(x) -> x
             | None -> null
        ignore x