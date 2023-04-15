---
category: Contributors
categoryindex: 2
index: 11
---

# Fantomas is trying to format the input multiple times due to the detection of multiple defines

As explained in [Formatting Conditional Compilation Directives](./Conditional%20Compilation%20Directives.html), Fantomas will try to format the input multiple times if it detects multiple defines.  
The amount of conditional directives should be exactly the same in each pass. This is a requirement in order for Fantomas to merge the results into one.  
Unfortunately, this is not always the case and an exception will be thrown if the bookkeeping doesn't add up.

As a case-study, we will look at issue [#2844](https://github.com/fsprojects/fantomas/issues/2844) and see how we troubleshoot these types of issues.

```
System.FormatException: Fantomas is trying to format the input multiple times due to the detection of multiple defines.
There is a problem with merging all the code back together.
[] has 7 fragments
[IOS] has 9 fragments
```

## Isolate each define combination

The first step is to isolate each define combination in a unit test. Doing this will simplify the debugging process.  
The `formatSourceStringWithDefines` can be used to only format the input with a specific set of defines.

```fsharp
[<Test>]
let ``good unit test name, no defines`` () =
    formatSourceStringWithDefines
        []
        """
                        program.SyncAction
                            (
#if IOS
                            // iOS animates by default layout changes, we don't want that
                            fun () -> v
#else
                            fn
#endif
                        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
program.SyncAction(
#if IOS
#else
    fn
#endif
)
"""
```

Notice that our result code should reflect only the active code branches.
`IOS` is not present and so no code is expected between `#if IOS` and `#else`.

If we do this for each combination, we can narrow the problem down to find the troublesome combination.

```fsharp
[<Test>]
let ``good unit test name, IOS`` () =
    formatSourceStringWithDefines
        [ "IOS" ]
        """
                        program.SyncAction
                            (
#if IOS
                            // iOS animates by default layout changes, we don't want that
                            fun () -> v
#else
                            fn
#endif
                        )
"""
        config
    |> prepend newline
    |> fun r ->
        printfn "%s" r
        r
    |> should
        equal
        """
program.SyncAction
    (
#if IOS
    // iOS animates by default layout changes, we don't want that
    fun () -> v
#else
#endif
    )
"""
```

![Result in test explorer](../../images/multiple-times-test-explorer.png)

## Bringing it all together

If each combination is fixed, we can now try to format the input with all the defines.  
Notice that we use `formatSourceString` instead of `formatSourceStringWithDefines` here.

```fsharp
[<Test>]
let ``good unit test name, 2844`` () =
    formatSourceString
        false
        """
                        program.SyncAction
                            (
#if IOS
                            // iOS animates by default layout changes, we don't want that
                            fun () -> v
#else
                            fn
#endif
                        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
program.SyncAction
    (
#if IOS
    // iOS animates by default layout changes, we don't want that
    fun () -> v
#else
    fn
#endif
    )
"""
```

## Unit test naming conventions

When dealing with multiple defines, it is important to name the unit tests in a way that makes it easy to understand what is going on.  
Use the following naming convention suffix:

- `, no defines` for the `[]` case
- `, defineA defineB` for the `[ "defineA"; "defineB" ]` case
- `, issue-number` for the full test.

<fantomas-nav previous="./The%20Missing%20Comment.html" next="./Pull%20request%20ground%20rules.html"></fantomas-nav>