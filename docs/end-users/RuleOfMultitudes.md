---
category: End-users
categoryindex: 1
index: 18
---

# The rule of multitudes

Often more than one layout of the same code parses. When Fantomas has to choose, it prefers the one
where every line it starts sits on a multiple of `indent_size`: a continuation goes in by whole
levels, never to a column in between. Only where a line starts counts; where a token lands further
along a line does not.

Why:

* **The style guides ask for it.** Both the
[Microsoft](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#use-consistent-indentation)
and the [G-Research](https://github.com/G-Research/fsharp-formatting-conventions#general-rules-for-indentation)
guide count indentation in spaces "per indentation", and put "consistency of indentation" first.
* **A level reads as structure.** With an indent size of 4, a line at column 10 sits between two
levels and reads as a mistake.
* **It does not depend on names.** A column taken from a token moves when the name in front of it
gets longer or shorter, and every line aligned to it moves along. The Microsoft guide says the
same in
[Avoid formatting that is sensitive to name length](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#avoid-formatting-that-is-sensitive-to-name-length).

8.1 moved the lines of a return type off the column after `:` for this reason:

```fsharp
// v8.0: the `->` lines at column 14, between two levels
let f
    x
    : (LongTypeNameNumberOne
              -> LongTypeNameNumberTwo
              -> LongTypeNameNumberThree
              -> LongTypeNameNumberFour
              -> LongTypeNameNumberFive)
    =
    failwith ""

// v8.1: one level past the binding, column 8
let f
    x
    : (LongTypeNameNumberOne
        -> LongTypeNameNumberTwo
        -> LongTypeNameNumberThree
        -> LongTypeNameNumberFour
        -> LongTypeNameNumberFive)
    =
    failwith ""
```

## When the parser needs a line past a column

The offside rule sometimes forbids a column. A parenthesis, for one, opens a context at the first
token inside it, and a line that starts on that column ends it. Fantomas does not then align to
the column after it: it steps in whole levels until the line is past it. `indentPast` in
`Context.fs` does that measuring and stepping.

```fsharp
type T =
    a
        -> (LongTypeNameNumberOne
                -> LongTypeNameNumberTwo // 12, a multiple too, would end the parenthesis; 16 is the next
                -> LongTypeNameNumberThree
                -> LongTypeNameNumberFour
                -> LongTypeNameNumberFive)
```

## Applying it

When two layouts parse, write down the column each line starts on, and prefer the layout where
every one of them is a multiple of `indent_size`. Try `indent_size = 2` and `3` as well: a layout
that lands on the grid only with 4 does not follow the rule, it follows the number 4. This is how a
new layout for Fantomas is judged, so it is the check to make when you propose one.

## Where it does not hold

A few layouts align to a token on purpose or for now. A new layout should not add to them without a
reason written down.

* `fsharp_multiline_bracket_style = cramped` puts the elements of a list under the first one, two
columns past the `[`:
  
  ```fsharp
  let values =
      [ someLongValueExpressionThatIsLongNumberOne
        someLongValueExpressionThatIsLongNumberTwo
        someLongValueExpressionThatIsLongNumberThree ]
  ```
  

* The closing `>` of a multiline type application lands one column past the indent:
  
  ```fsharp
  type T =
      Dictionary<
          LongTypeNameNumberOne,
          LongTypeNameNumberTwo * LongTypeNameNumberThree * LongTypeNameNumberFour * LongTypeNameNumberFive
       >
  ```
  

<fantomas-nav source="docs/end-users/RuleOfMultitudes.md" previous="ConditionalCompilationDirectives.md" next="ReportingBugs.md"></fantomas-nav>