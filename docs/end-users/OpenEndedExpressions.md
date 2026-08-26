---
category: End-users
categoryindex: 1
index: 3
---

# Open-ended expressions

Sometimes Fantomas leaves a short record, list or tuple spread over several lines when it would
comfortably fit on one. This page explains when that happens and why, because it is deliberate
rather than a missed opportunity.

## The symptom

This record is well under the width Fantomas would collapse at, and it stays as it is:

```fsharp
let r =
    {
        A = 1
        B = fun x -> x + 1
        C = 3
    }
```

Take the lambda out and the same record goes to one line:

```fsharp
let r = { A = 1; B = 2; C = 3 }
```

## Why

In F#, `fun` extends as far to the right as it can. That is a rule of the language, not a
formatting preference. Written on one line, the record above says something else entirely:

```fsharp
let r = { A = 1; B = fun x -> x + 1; C = 3 }
```

The compiler reads that as a record with **two** fields, not three, because everything after
the arrow belongs to the lambda:

```fsharp
let r = { A = 1; B = fun x -> (x + 1; C = 3) }
```

The `;` that Fantomas would write to separate the fields becomes a sequential expression inside
the lambda body instead.

[Fantomas re-types your entire source](https://fsprojects.github.io/fantomas/docs/end-users/StyleGuide.html#How-Fantomas-formats-code) rather than
adjusting whitespace in place, so it carries the responsibility that what it prints means what
you wrote. Collapsing here would break that, so it does not collapse.

## Where it applies

An expression is *open-ended* when it has no closing token and so runs on to the right. The
common ones are `fun`, `if ... then ... else`, `match`, `function` and `try`, along with
anything that wraps one of them, such as `lazy`, `yield` or an assignment.

Fantomas keeps the layout multiline when an open-ended expression sits anywhere but **last** in
a record, an anonymous record, a list, an array or a tuple, or on the **left** of an infix
operator:

```fsharp
let xs =
    [
        1, fun () -> 1
        2, fun () -> 2
    ]

let a =
    fun x -> x + 1
    |> g
```

The last position is fine as it is, because the closing bracket ends the expression and there is
nothing left for it to swallow:

```fsharp
let r = { A = 1; C = 3; B = fun x -> x + 1 }
```

What counts is whether the expression really does run on. `lazy` around a lambda does, `lazy`
around a plain value does not, so only the first of these stays multiline:

```fsharp
let xs =
    [
        lazy fun x -> x
        2
    ]

let ys = [ lazy a; 2 ]
```

## Getting the single line back

Put the parentheses in yourself. They close the expression, so nothing can run past them and
Fantomas is free to collapse:

```fsharp
let r = { A = 1; B = (fun x -> x + 1); C = 3 }

let xs = [ 1, (fun () -> 1); 2, (fun () -> 2) ]
```

This is a change to your code rather than to your formatting, which is exactly why Fantomas
leaves it to you.

## Why not add the parentheses for you

That was the other option, and it was put to the community as
[RFC #3279](https://github.com/fsprojects/fantomas/issues/3279). Both options produce
deterministic output; the question was only which one. The vote went 23 to 3 in favour of
staying multiline, on the grounds that a formatter should not introduce syntax that was not in
the source, and Fantomas has behaved this way since `v8.0.0-alpha-008`.

Note that neither option is about preserving what you wrote. Written on one line to begin with,
the parenthesised record above stays on one line; written across several, it is collapsed onto
one. What decides the layout is the expression, never the way you happened to type it.

<fantomas-nav previous="StyleGuide.md" next="Configuration.md"></fantomas-nav>