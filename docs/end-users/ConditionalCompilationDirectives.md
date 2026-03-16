# Conditional Compilation Directives

Fantomas supports formatting F# code that contains conditional compilation directives (`#if`, `#else`, `#endif`).
However, there is an important limitation to be aware of.

## How Fantomas handles directives

Fantomas needs to parse your code into an abstract syntax tree (AST) before it can format it.
The F# parser processes `#if` / `#else` / `#endif` directives at parse time, meaning it picks one branch based on which defines are active and ignores the other.

To handle this, Fantomas:

0 Parses your code without any defines to discover all conditional directives.

1 Determines every possible combination of defines.

2 Parses and formats the code once for each combination.

3 Merges the results back together.

## The limitation: all define combinations must produce valid syntax

Because Fantomas parses your code under **every** define combination, **each combination must result in a valid syntax tree**.

For example, the following code **cannot** be formatted:

```fsharp
module F =
    let a: string =
        #if FOO
        ""
        #endif
        #if BAR
        "a"
        #endif

    let baz: unit = ()
```

When neither `FOO` nor `BAR` is defined, the code becomes:

```fsharp
module F =
    let a: string =

    let baz: unit = ()
```

This is not valid F# — `let a` has no body — so the parser raises an error and Fantomas cannot proceed.

## How to fix it

Make sure that every combination of defines still produces valid F# code. The most common fix is to add an `#else` branch:

```fsharp
module F =
    let a: string =
        #if FOO
        ""
        #else
        "a"
        #endif

    let baz: unit = ()
```

Now, regardless of whether `FOO` is defined, the parser always sees a complete `let` binding.

## Using `.fantomasignore`

If you cannot restructure the directives (e.g. because the code is generated or must match a particular pattern), you can exclude the file from formatting using a [`.fantomasignore`](https://fsprojects.github.io/fantomas/docs/end-users/IgnoreFiles.html) file.

<fantomas-nav previous="Recipes.md" next="FAQ.md"></fantomas-nav>