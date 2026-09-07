module internal Fantomas.Core.MultipleDefineCombinations

/// When conditional defines were found in the source code, we format the code using all possible combinations.
/// Depending on the values of each combination, code will either be produced or not.
/// In this function, we try to piece back all the active code fragments.
///
/// The merge works on the formatted text, not on the tree. Each result is split into fragments at
/// its `#if`, `#else` and `#endif` lines, so every result must have the same number of fragments;
/// when they do not, the merge raises `FormatException`. Fragments are then compared position by
/// position and the one with the most lines wins, an empty branch losing to a non-empty one.
/// The cursor, if any, is taken from the result whose winning fragment contained it.
///
/// Because the split is by line, CodePrinter has to print a directive on a line of its own and may
/// not move code across one. Illustrated in src/Fantomas.Core.Tests/MultipleDefineCombinationsTests.fs.
val mergeMultipleFormatResults: config: FormatConfig -> results: (DefineCombination * FormatResult) list -> FormatResult
