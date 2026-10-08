---
category: Contributors
categoryindex: 2
index: 13
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

The first step is to look at each define combination on its own. Doing this will simplify the debugging process.
Put the input in a snapshot case, in the folder of the node it is about, named after the issue:
`src/Fantomas.Core.SnapshotTests/cases/oak/Expr/Chain/trivia/2844-directive-in-parenthesis-argument.fs`.
That path is where the case would go, not one in the repository: create the file, and the `trivia/` folder it sits in, to follow along.
The [snapshot tests' README](https://github.com/fsprojects/fantomas/blob/main/src/Fantomas.Core.SnapshotTests/README.md) says where a case goes.

```fsharp
program.SyncAction
    (
#if IOS
    // iOS animates by default layout changes, we don't want that
    fun () -> v
#else
    fn
#endif
    )
```

`scripts/format.fsx` formats one combination at a time when it is given `--define`, and prints the result for that combination before the merge.
`no-defines` is the combination without any.

```shell
dotnet build src/Fantomas.Core.SnapshotTests
dotnet fsi scripts/format.fsx --define no-defines src/Fantomas.Core.SnapshotTests/cases/oak/Expr/Chain/trivia/2844-directive-in-parenthesis-argument.fs
dotnet fsi scripts/format.fsx --define IOS src/Fantomas.Core.SnapshotTests/cases/oak/Expr/Chain/trivia/2844-directive-in-parenthesis-argument.fs
```

Each result should reflect only the active code branches.
`IOS` is not defined in the first, so no code is expected between `#if IOS` and `#else`:

```fsharp
program.SyncAction(
    #if IOS
    #else
    fn
#endif
)
```

With `IOS` defined, the code between `#else` and `#endif` is gone instead:

```fsharp
program.SyncAction(
    #if IOS
    // iOS animates by default layout changes, we don't want that
    fun () -> v
#else
#endif
)
```

A directive is printed where the code around it puts it, which is why some are indented here; the merge moves every one of them to the start of its line.
What the merge needs is that every combination has its directives on lines of their own, as many of them, in the same order.
It splits each result at its directives and stitches the pieces back together, so a combination that puts code on the line of a directive, or moves code across one, is the one to fix.
If we do this for each combination, we can narrow the problem down to find the troublesome combination.

## Bringing it all together

Once every combination gives its directives that way, the merge succeeds and the case can get its golds:

```shell
FANTOMAS_UPDATE_SNAPSHOTS=1 dotnet test src/Fantomas.Core.SnapshotTests --filter "Name~2844-directive-in-parenthesis-argument"
```

That writes a gold per combination, `2844-directive-in-parenthesis-argument.no-defines.gold.fs` and `2844-directive-in-parenthesis-argument.IOS.gold.fs`, which hold the results above, and `2844-directive-in-parenthesis-argument.gold.fs`, the merged result users get:

```fsharp
program.SyncAction(
#if IOS
    // iOS animates by default layout changes, we don't want that
    fun () -> v
#else
    fn
#endif
)
```

Read all of them: they are what the fix pins down.

<fantomas-nav source="{{fsdocs-source-filename}}" previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>