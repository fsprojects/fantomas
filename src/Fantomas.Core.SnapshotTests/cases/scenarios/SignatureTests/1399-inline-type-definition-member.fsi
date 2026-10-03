namespace Baz

[<Sealed>]
type Foo =
    member inline Return : 'a -> Baz<'a>
