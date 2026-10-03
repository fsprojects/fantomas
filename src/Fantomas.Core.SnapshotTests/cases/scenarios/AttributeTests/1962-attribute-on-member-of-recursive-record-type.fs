(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = cramped
---*)
module Foo =

    type Person = {
        Name : string
        FavoriteDog : Dog
    } with
        [<RequiresExplicitTypeArguments>]
        static member doThing person =
            ()
    and Dog = {
        Name : string
        FavoriteChewToy : string
    } with
        [<RequiresExplicitTypeArguments>]
        static member doThing person =
            ()
