(*---
# A `#if` inside a string is taken for a directive, which moves it to column 0 and changes the
# string. Not supported for now.
---*)
#if FOO
    """
    #if BAR
                printfn "FOO"
    #endif
    """
#else
                ()
#endif
