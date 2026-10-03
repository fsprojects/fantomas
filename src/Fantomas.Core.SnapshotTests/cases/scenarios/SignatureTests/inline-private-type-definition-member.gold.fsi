namespace Baz

[<Sealed>]
type Foo =
    member inline private Return: 'a -> Baz<'a>
