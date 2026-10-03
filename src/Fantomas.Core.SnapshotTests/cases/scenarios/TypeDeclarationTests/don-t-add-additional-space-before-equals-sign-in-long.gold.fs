type LdapClaimsTransformation
    (
        ldapSearcher: ILdapSearcher,
        options: ILdapClaimsTransformationOptions
    )
    =

    interface IClaimsTransformation with
        member __.TransformAsync principle = 3
