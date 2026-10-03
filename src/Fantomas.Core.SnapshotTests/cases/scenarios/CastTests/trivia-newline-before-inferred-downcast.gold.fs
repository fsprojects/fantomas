namespace Blah

module Foo =

    let foo =
        { new IDisposable with
            member __.Dispose() =
                do ()

                downcast () }
