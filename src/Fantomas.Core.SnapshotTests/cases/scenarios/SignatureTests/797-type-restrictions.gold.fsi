namespace Foo

type internal Foo2 =
    abstract member Bar<'k> : unit -> unit when 'k: comparison
