namespace Foo

open System.Linq

module Bar =
    let Baz () =
        for foo in bar().OfType<SomeType>() do
            printf "baz"

        for foo in bar().meh<SomeType>() do
            printf "baz"
