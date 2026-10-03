(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
type Foo () =
    interface IDisposable with
        override __.Dispose () =
            if not blah then
                ()
            else

            try
                try
                    cleanUp ()
                with
                | :? IOException ->
                    foo ()
            with exc ->
                foooo ()
