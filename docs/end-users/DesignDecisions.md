---
category: End-users
categoryindex: 1
index: 14
---

# Design decisions

Fantomas rewrites the entire source text after formatting. Think of it like a word document:
Fantomas will re-type your entire text according to its rules in a new file. It does not modify
the original text. This approach ensures complete consistency and adherence to the formatting
rules, but it means that all formatting decisions are made by Fantomas according to its opinionated
style guide.

Because the output has to mean what the input meant, Fantomas sometimes behaves in a way that can
look surprising at first: keeping code multiline that would fit on one line, refusing to format a
file at all, or laying out a chain of calls in a way you would not have typed by hand. None of this
is a bug or a missed opportunity. Each case below explains the reasoning behind one of those
behaviors.

## Open-ended expressions

Sometimes Fantomas leaves a short record, list or tuple spread over several lines when it would
comfortably fit on one, because collapsing it would silently change what the code means.
See [Open-ended expressions](./OpenEndedExpressions.html).

## Formatting chain expressions

A chain of dotted member access and calls, such as `document.Body.FirstChild.AppendChild(newNode)`,
is one of the few shapes where Fantomas has a real choice to make about where line breaks go.
See [Formatting chain expressions](./Chains.html).

## Conditional compilation directives

Fantomas formats code under every combination of `#if` / `#else` / `#endif` defines and merges the
results, which means every combination has to produce valid F# on its own.
See [Conditional Compilation Directives](./ConditionalCompilationDirectives.html).

<fantomas-nav previous="StyleGuide.md" next="Chains.md"></fantomas-nav>