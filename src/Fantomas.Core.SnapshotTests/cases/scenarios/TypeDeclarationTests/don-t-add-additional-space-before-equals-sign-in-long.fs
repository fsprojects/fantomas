(*---
max_line_length = 60
fsharp_alternative_long_member_definitions = true
---*)
type LdapClaimsTransformation(
                                 ldapSearcher : ILdapSearcher,
                                 options : ILdapClaimsTransformationOptions
                             ) =

    interface IClaimsTransformation with
        member __.TransformAsync principle =
            3
