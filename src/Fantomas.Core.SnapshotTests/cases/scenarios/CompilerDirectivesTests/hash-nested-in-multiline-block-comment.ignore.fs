(*---
# A `#if` inside a block comment is taken for a directive, which moves it to column 0 inside the
# comment. Not supported for now.
---*)
#if FOO
    (*
        #if BAR
                    printfn "FOO"
        #endif
    *)
#else
                ()
#endif
