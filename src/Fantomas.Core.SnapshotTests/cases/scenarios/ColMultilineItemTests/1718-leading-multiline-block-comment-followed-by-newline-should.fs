(*
    My personal favorite: Discriminated Unions!
    This is a feature related to sum types in category theory and incredibly useful for code correctness.
    ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions
*)

type Days = Days of int  // you can alias types in F# to get a DDD kind of vibe.
type StoryPoints = StoryPoints of int
type Money = Money of double
