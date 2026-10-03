(*---
fsharp_space_before_colon = true
---*)
let private userNameDecoder (get : Decode.IGetters) =
    let givenName =
        get.Optional.Field "given_name" Decode.string

    let familyName =
        get.Optional.Field "family_name" Decode.string

    match givenName, familyName with
    | Some g, Some f -> sprintf "%s %c" g f.[0]
    | _ -> get.Required.Field "nickname" Decode.string
