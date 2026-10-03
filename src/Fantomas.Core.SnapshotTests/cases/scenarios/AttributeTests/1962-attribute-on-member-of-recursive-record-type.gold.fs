module Foo =

    type Person =
        { Name: string
          FavoriteDog: Dog }
        [<RequiresExplicitTypeArguments>]
        static member doThing person = ()

    and Dog =
        { Name: string
          FavoriteChewToy: string }
        [<RequiresExplicitTypeArguments>]
        static member doThing person = ()
