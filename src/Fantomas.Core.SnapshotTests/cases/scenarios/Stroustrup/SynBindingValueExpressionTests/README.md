# SynBindingValueExpressionTests

These cases were the tests of [`src/Fantomas.Core.Tests/Stroustrup/SynBindingValueExpressionTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/Stroustrup/SynBindingValueExpressionTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../../README.md) says how to read them.

## hash-define-before-closing-list-bracket

Hash directives (#if/#endif) are assigned as ContentBefore of the closing `]` during trivia assignment. This forces the list into aligned bracket layout instead of Stroustrup, because the directive resets indentation to column 0 which would break the offside rule with an inline `[`.

Written above:

- [`hash-define-before-closing-list-bracket.fs`](hash-define-before-closing-list-bracket.fs)

## comment before closing list bracket with hash directive, something defined

The following tests cover an edge case where a line comment sits after #endif
but before the closing bracket. The core problem:

  1. findNodeBeforeWithMatchingColumn matches "item1" (column 4) for the comment (column 4),
     assigning it as ContentAfter on item1; but #else/#endif sit between them in the source.
  2. The directives are assigned as ContentBefore on ] (they go through the default path).
  3. This reverses the source order: the comment (line 8) is emitted before #else (line 5).

After formatting, directives move to column 0, so on the second pass the comment is no longer
at the same column as the preceding item, breaking the column-matching heuristic and causing
the comment to shift between passes (not idempotent).

A proper fix would need findNodeBeforeWithMatchingColumn to be aware of directive boundaries:
if a #if/#else/#endif sits between the candidate node and the comment, the match is invalid.
This is a very specific interaction between the column-matching trivia assignment and the
multi-define formatting pipeline.

Written above these cases:

- `comment before closing list bracket with hash directive, something defined`: is ignored, the harness result differs from the expected one, and formats one define combination, which no gold holds
- `comment before closing list bracket with hash directive, nothing defined`: is ignored, the harness result differs from the expected one, and formats one define combination, which no gold holds
- [`comment-before-closing-list-bracket-with-hash-directive.ignore.fs`](comment-before-closing-list-bracket-with-hash-directive.ignore.fs)
