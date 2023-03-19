module internal Fantomas.Core.MultipleDefineCombinations

/// When conditional defines were found in the source code, we format the code using all possible combinations.
/// Depending on the values of each combination, code will either be produced or not.
/// In this function, we try to piece back all the active code fragments.
val mergeMultipleFormatResults: config: FormatConfig -> results: (DefineCombination * FormatResult) list -> FormatResult
