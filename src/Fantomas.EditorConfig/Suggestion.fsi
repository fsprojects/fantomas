module Fantomas.Suggestion

/// How many single character edits turn one string into the other, stopping once the answer is
/// known to be above `limit`.
val editDistance: limit: int -> left: string -> right: string -> int

/// The candidate closest to `term`, when one is within `limit` edits of it, and `None` when the
/// nearest is far enough away that naming it would be a guess rather than help.
///
/// Two candidates the same distance away are separated by the order of `candidates`, since
/// `List.sortBy` is stable.
val nearest: limit: int -> candidates: string list -> term: string -> string option
