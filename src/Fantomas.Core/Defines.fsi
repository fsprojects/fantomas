namespace Fantomas.Core

/// One set of defines the source is parsed and formatted under, such as `[ "DEBUG"; "NET" ]`.
/// A file with conditional directives is formatted once per combination and the results merged,
/// see `MultipleDefineCombinations`.
type internal DefineCombination =
    | DefineCombination of defines: string list

    member Value: string list

    static member Empty: DefineCombination

module internal Defines =
    open Fantomas.FCS.SyntaxTrivia

    /// The combinations that together make every branch of every `#if` active at least once.
    /// Always the empty set first, then sets a solver derived from the directive expressions,
    /// then each define on its own. Illustrated in src/Fantomas.Core.Tests/DefinesTests.fs.
    val getDefineCombination: hashDirectives: ConditionalDirectiveTrivia list -> DefineCombination list
