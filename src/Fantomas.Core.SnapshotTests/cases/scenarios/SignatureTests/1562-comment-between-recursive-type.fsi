namespace Baz

type Foo = | Foo of int

///
and [<RequireQualifiedAccess>] Bar<'a> =
    | Bar of int
