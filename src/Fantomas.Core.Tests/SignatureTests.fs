module Fantomas.Core.Tests.SignatureTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

// the current behavior results in a compile error since "(string * string) list" is converted to "string * string list"
[<Test>]
let ``should keep the (string * string) list type signature in records`` () =
    formatSourceString
        false
        """type MSBuildParams =
    { Targets : string list
      Properties : (string * string) list
      MaxCpuCount : int option option
      ToolsVersion : string option
      Verbosity : MSBuildVerbosity option
      FileLoggers : MSBuildFileLoggerConfig list option }

    """
        config
    |> should
        equal
        """type MSBuildParams =
    { Targets: string list
      Properties: (string * string) list
      MaxCpuCount: int option option
      ToolsVersion: string option
      Verbosity: MSBuildVerbosity option
      FileLoggers: MSBuildFileLoggerConfig list option }
"""

[<Test>]
let ``should keep the (string * string) list type signature in functions`` () =
    formatSourceString
        false
        """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> (string * string) list) projects =
    doingsomstuff
"""
        config
    |> prepend newline
    |> should
        equal
        """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> (string * string) list) projects =
    doingsomstuff
"""

[<Test>]
let ``should keep the string * string list type signature in functions`` () =
    formatSourceString
        false
        """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: (string -> string) * string list) projects =
    doingsomstuff
"""
        config
    |> prepend newline
    |> should
        equal
        """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: (string -> string) * string list) projects =
    doingsomstuff
"""

[<Test>]
let ``should not add parens in signature`` () =
    formatSourceString
        false
        """type Route =
    { Verb : string
      Path : string
      Handler : Map<string, string> -> HttpListenerContext -> string }
    override x.ToString() = sprintf "%s %s" x.Verb x.Path

    """
        { config with
            MaxFunctionBindingWidth = 120
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> should
        equal
        """type Route =
    { Verb: string
      Path: string
      Handler: Map<string, string> -> HttpListenerContext -> string }
    override x.ToString() = sprintf "%s %s" x.Verb x.Path
"""

[<Test>]
let ``should keep the string * string * string option type signature`` () =
    formatSourceString
        false
        """type DGML =
    | Node of string
    | Link of string * string * (string option)

    """
        config
    |> should
        equal
        """type DGML =
    | Node of string
    | Link of string * string * (string option)
"""

[<Test>]
let ``should keep the (string option * Node) list type signature`` () =
    formatSourceString
        false
        """type Node =
    { Name : string;
      NextNodes : (string option * Node) list }

    """
        config
    |> should
        equal
        """type Node =
    { Name: string
      NextNodes: (string option * Node) list }
"""

[<Test>]
let ``should keep parentheses on the left of type signatures`` () =
    formatSourceString
        false
        """type IA =
    abstract F: (unit -> Option<'T>) -> Option<'T>

type A () =
    interface IA with
        member x.F (f: unit -> _) = f ()
    """
        config
    |> prepend newline
    |> should
        equal
        """
type IA =
    abstract F: (unit -> Option<'T>) -> Option<'T>

type A() =
    interface IA with
        member x.F(f: unit -> _) = f ()
"""

[<Test>]
let ``should not add parentheses around bare tuples`` () =
    formatSourceString
        true
        """
namespace TupleType
type C =
    member P1 : int * string
    /// def
    member P2 : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace TupleType

type C =
    member P1: int * string
    /// def
    member P2: int
"""

[<Test>]
let ``should keep global constraints in type signature`` () =
    formatSourceString
        true
        """
module Tainted
val GetHashCodeTainted : (Tainted<'T> -> int) when 'T : equality
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Tainted

val GetHashCodeTainted: (Tainted<'T> -> int) when 'T: equality
"""

[<Test>]
let ``should keep mutable in type signature, 1954`` () =
    formatSourceString
        true
        """
module Tainted
val mutable showParserStackOnParseError: bool
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Tainted

val mutable showParserStackOnParseError: bool
"""

[<Test>]
let ``should keep access modifiers in signatures seperated`` () =
    formatSourceString
        true
        """
module Test
type Test =
    static member internal FormatAroundCursorAsync : fileName:string -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Test

type Test =
    static member internal FormatAroundCursorAsync: fileName: string -> unit
"""

[<Test>]
let ``comment should stay above type`` () =
    formatSourceString
        true
        """namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>
"""

[<Test>]
let ``comment before namespace should be preserved`` () =
    formatSourceString
        true
        """
// some comment
namespace TypeEquality

type Teq<'a, 'b>
"""
        config
    |> prepend newline
    |> should
        equal
        """
// some comment
namespace TypeEquality

type Teq<'a, 'b>
"""

[<Test>]
let ``generic val in nested module should keep generic type parameters`` () =
    formatSourceString
        true
        """namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>

/// Module for creating and using type equalities, primarily useful for Generalised Algebraic Data Types (GADTs)
/// Invariant: If you use this module (without reflection shenanigans) and the
/// code builds, it will be correct.
[<RequireQualifiedAccess>]
module Teq =

    /// The single constructor for Teq - witnesses equality between 'a and 'a
    /// It would be nice to accept any isomorphism (i.e. 1-1 mapping between
    /// values, but Refl is the only provably correct constructor we can create
    /// in F#, so we choose soundness over completeness here).
    val refl<'a> : Teq<'a, 'a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>

/// Module for creating and using type equalities, primarily useful for Generalised Algebraic Data Types (GADTs)
/// Invariant: If you use this module (without reflection shenanigans) and the
/// code builds, it will be correct.
[<RequireQualifiedAccess>]
module Teq =

    /// The single constructor for Teq - witnesses equality between 'a and 'a
    /// It would be nice to accept any isomorphism (i.e. 1-1 mapping between
    /// values, but Refl is the only provably correct constructor we can create
    /// in F#, so we choose soundness over completeness here).
    val refl<'a> : Teq<'a, 'a>
"""

[<Test>]
let ``don't duplicate newline between type and module`` () =
    formatSourceString
        true
        """namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>

/// Module for creating and using type equalities, primarily useful for Generalised Algebraic Data Types (GADTs)
/// Invariant: If you use this module (without reflection shenanigans) and the
/// code builds, it will be correct.
[<RequireQualifiedAccess>]
module Teq =

    /// The single constructor for Teq - witnesses equality between 'a and 'a
    /// It would be nice to accept any isomorphism (i.e. 1-1 mapping between
    /// values, but Refl is the only provably correct constructor we can create
    /// in F#, so we choose soundness over completeness here).
    val refl<'a> : Teq<'a, 'a>

    /// Returns a Teq when the two type parameters have the same value,
    /// otherwise returns None.
    val tryRefl<'a, 'b> : Teq<'a, 'b> option

    /// Order isn't important
    /// a = b => b = a
    /// If you always do this followed by a cast, you may as well just use castFrom
    val symmetry: Teq<'a, 'b> -> Teq<'b, 'a>

    /// Let's compose two type-equalities: a = b && b = c => a = c
    val transitivity: Teq<'a, 'b> -> Teq<'b, 'c> -> Teq<'a, 'c>

    /// Converts an 'a to a 'b
    val cast: Teq<'a, 'b> -> ('a -> 'b)

    /// Converts an 'a to a 'b
    /// Alias for cast
    val castTo: Teq<'a, 'b> -> ('a -> 'b)

    /// Converts a 'b to an 'a
    /// Equivalent to symmetry >> cast, but more efficient
    val castFrom: Teq<'a, 'b> -> ('b -> 'a)

    /// Utility function to map an object of one type using a mapping function
    /// for a different type when we have a type equality between the two types
    val mapAs: Teq<'a, 'b> -> ('b -> 'b) -> 'a -> 'a

    /// The Cong module (short for congruence) contains functions that
    /// allow you safely transform Teqs into other Teqs that logically follow.
    ///
    /// Congruence: if x = y then f x = f y
    /// From a type-level perspective, this means, for example,
    /// iff 'a = 'b, then 'a list = 'b list.
    ///
    /// We do the munging below since we don't have type-level functions, so there
    /// is no such thing as, for example, Teq.cong List, to raise a
    /// Teq<'a, 'b> to a Teq<'a list, 'b list>. Instead we must create a function for
    /// any functor (wrapping type) we might want, e.g. list, option, array.
    ///
    /// More efficient than mapping the application of the teq across the functor.
    /// i.e. Use Teq.Cong.list in preference to List.map (Teq.cast)
    ///
    /// If you need to make your own Teq.Cong.foo for your own functor 'Foo<_>' using believeMe,
    /// then the onus is on you to verify that doing that is sane.
    [<RequireQualifiedAccess>]
    module Cong =

        /// Clearly unsafe in general, but safe if we know 'a = 'b (which the Teq proves),
        /// and that there is some f such that f 'a = 'a2 = f 'b = 'b2, which we assume (and
        /// this assumption is why we don't make this public). Examples of valid values for
        /// f include list, array and option.
        val believeMe<'a, 'b, 'a2, 'b2> : Teq<'a, 'b> -> Teq<'a2, 'b2>

        /// Given a type equality between two types, returns the type equality on the corresponding array types.
        val array<'a, 'b> : Teq<'a, 'b> -> Teq<'a array, 'b array>

        /// Given a type equality between two array types, returns the type equality on the corresponding element types.
        val arrayOf<'a, 'b> : Teq<'a array, 'b array> -> Teq<'a, 'b>

        /// Given a type equality between two types, returns the type equality on the corresponding list types.
        val list<'a, 'b> : Teq<'a, 'b> -> Teq<'a list, 'b list>

        /// Given a type equality between two list types, returns the type equality on the corresponding element types.
        val listOf<'a, 'b> : Teq<'a list, 'b list> -> Teq<'a, 'b>

        /// Given a type equality between two types, returns the type equality on the corresponding option types.
        val option<'a, 'b> : Teq<'a, 'b> -> Teq<'a option, 'b option>

        /// Given a type equality between two option types, returns the type equality on the corresponding element types.
        val optionOf<'a, 'b> : Teq<'a option, 'b option> -> Teq<'a, 'b>

        /// Given a type equality between two types 'domain1 and 'domain2, returns the type equality
        /// on the function types ('domain1 -> 'range) and ('domain2 -> 'range), for any arbitrary 'range.
        val domain<'domain1, 'domain2, 'range> : Teq<'domain1, 'domain2> -> Teq<'domain1 -> 'range, 'domain2 -> 'range>

        /// Given a type equality between two function types, returns the type equality on their corresponding domains.
        val domainOf<'domain1, 'domain2, 'range1, 'range2> : Teq<'domain1 -> 'range1, 'domain2 -> 'range2> -> Teq<'domain1, 'domain2>

        /// Given a type equality between two types 'range1 and 'range2, returns the type equality
        /// on the function types ('domain -> 'range1) and ('domain -> 'range2), for any arbitrary 'domain.
        val range<'domain, 'range1, 'range2> : Teq<'range1, 'range2> -> Teq<'domain -> 'range1, 'domain -> 'range2>

        /// Given a type equality between two function types, returns the type equality on their corresponding ranges.
        val rangeOf<'domain1, 'domain2, 'range1, 'range2> : Teq<'domain1 -> 'range1, 'domain2 -> 'range2> -> Teq<'range1, 'range2>

        /// Given a pair of type equalities, one for domains and one for ranges, returns the type equality for the corresponding function types.
        val func<'domain1, 'range1, 'domain2, 'range2> : Teq<'domain1, 'domain2> -> Teq<'range1, 'range2> -> Teq<'domain1 -> 'range1, 'domain2 -> 'range2>

        /// Given a type equality between two types 'fst1 and 'fst2, returns the type equality
        /// on the pair types ('fst1 * 'snd) and ('fst2 * 'snd), for any arbitrary 'snd.
        val fst<'fst1, 'fst2, 'snd> : Teq<'fst1, 'fst2> -> Teq<'fst1 * 'snd, 'fst2 * 'snd>

        /// Given a type equality between two types 'snd1 and 'snd2, returns the type equality
        /// on the pair types ('fst * 'snd1) and ('fst * 'snd2), for any arbitrary 'fst.
        val snd<'snd1, 'snd2, 'fst> : Teq<'snd1, 'snd2> -> Teq<'fst * 'snd1, 'fst * 'snd2>

        /// Given a pair of type equalities, one for the first element of a pair and one for the second element of a pair,
        /// returns the type equality for the corresponding pair types.
        val pair<'fst1, 'snd1, 'fst2, 'snd2> : Teq<'fst1, 'fst2> -> Teq<'snd1, 'snd2> -> Teq<'fst1 * 'snd1, 'fst2 * 'snd2>
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>

/// Module for creating and using type equalities, primarily useful for Generalised Algebraic Data Types (GADTs)
/// Invariant: If you use this module (without reflection shenanigans) and the
/// code builds, it will be correct.
[<RequireQualifiedAccess>]
module Teq =

    /// The single constructor for Teq - witnesses equality between 'a and 'a
    /// It would be nice to accept any isomorphism (i.e. 1-1 mapping between
    /// values, but Refl is the only provably correct constructor we can create
    /// in F#, so we choose soundness over completeness here).
    val refl<'a> : Teq<'a, 'a>

    /// Returns a Teq when the two type parameters have the same value,
    /// otherwise returns None.
    val tryRefl<'a, 'b> : Teq<'a, 'b> option

    /// Order isn't important
    /// a = b => b = a
    /// If you always do this followed by a cast, you may as well just use castFrom
    val symmetry: Teq<'a, 'b> -> Teq<'b, 'a>

    /// Let's compose two type-equalities: a = b && b = c => a = c
    val transitivity: Teq<'a, 'b> -> Teq<'b, 'c> -> Teq<'a, 'c>

    /// Converts an 'a to a 'b
    val cast: Teq<'a, 'b> -> ('a -> 'b)

    /// Converts an 'a to a 'b
    /// Alias for cast
    val castTo: Teq<'a, 'b> -> ('a -> 'b)

    /// Converts a 'b to an 'a
    /// Equivalent to symmetry >> cast, but more efficient
    val castFrom: Teq<'a, 'b> -> ('b -> 'a)

    /// Utility function to map an object of one type using a mapping function
    /// for a different type when we have a type equality between the two types
    val mapAs: Teq<'a, 'b> -> ('b -> 'b) -> 'a -> 'a

    /// The Cong module (short for congruence) contains functions that
    /// allow you safely transform Teqs into other Teqs that logically follow.
    ///
    /// Congruence: if x = y then f x = f y
    /// From a type-level perspective, this means, for example,
    /// iff 'a = 'b, then 'a list = 'b list.
    ///
    /// We do the munging below since we don't have type-level functions, so there
    /// is no such thing as, for example, Teq.cong List, to raise a
    /// Teq<'a, 'b> to a Teq<'a list, 'b list>. Instead we must create a function for
    /// any functor (wrapping type) we might want, e.g. list, option, array.
    ///
    /// More efficient than mapping the application of the teq across the functor.
    /// i.e. Use Teq.Cong.list in preference to List.map (Teq.cast)
    ///
    /// If you need to make your own Teq.Cong.foo for your own functor 'Foo<_>' using believeMe,
    /// then the onus is on you to verify that doing that is sane.
    [<RequireQualifiedAccess>]
    module Cong =

        /// Clearly unsafe in general, but safe if we know 'a = 'b (which the Teq proves),
        /// and that there is some f such that f 'a = 'a2 = f 'b = 'b2, which we assume (and
        /// this assumption is why we don't make this public). Examples of valid values for
        /// f include list, array and option.
        val believeMe<'a, 'b, 'a2, 'b2> : Teq<'a, 'b> -> Teq<'a2, 'b2>

        /// Given a type equality between two types, returns the type equality on the corresponding array types.
        val array<'a, 'b> : Teq<'a, 'b> -> Teq<'a array, 'b array>

        /// Given a type equality between two array types, returns the type equality on the corresponding element types.
        val arrayOf<'a, 'b> : Teq<'a array, 'b array> -> Teq<'a, 'b>

        /// Given a type equality between two types, returns the type equality on the corresponding list types.
        val list<'a, 'b> : Teq<'a, 'b> -> Teq<'a list, 'b list>

        /// Given a type equality between two list types, returns the type equality on the corresponding element types.
        val listOf<'a, 'b> : Teq<'a list, 'b list> -> Teq<'a, 'b>

        /// Given a type equality between two types, returns the type equality on the corresponding option types.
        val option<'a, 'b> : Teq<'a, 'b> -> Teq<'a option, 'b option>

        /// Given a type equality between two option types, returns the type equality on the corresponding element types.
        val optionOf<'a, 'b> : Teq<'a option, 'b option> -> Teq<'a, 'b>

        /// Given a type equality between two types 'domain1 and 'domain2, returns the type equality
        /// on the function types ('domain1 -> 'range) and ('domain2 -> 'range), for any arbitrary 'range.
        val domain<'domain1, 'domain2, 'range> : Teq<'domain1, 'domain2> -> Teq<'domain1 -> 'range, 'domain2 -> 'range>

        /// Given a type equality between two function types, returns the type equality on their corresponding domains.
        val domainOf<'domain1, 'domain2, 'range1, 'range2> :
            Teq<'domain1 -> 'range1, 'domain2 -> 'range2> -> Teq<'domain1, 'domain2>

        /// Given a type equality between two types 'range1 and 'range2, returns the type equality
        /// on the function types ('domain -> 'range1) and ('domain -> 'range2), for any arbitrary 'domain.
        val range<'domain, 'range1, 'range2> : Teq<'range1, 'range2> -> Teq<'domain -> 'range1, 'domain -> 'range2>

        /// Given a type equality between two function types, returns the type equality on their corresponding ranges.
        val rangeOf<'domain1, 'domain2, 'range1, 'range2> :
            Teq<'domain1 -> 'range1, 'domain2 -> 'range2> -> Teq<'range1, 'range2>

        /// Given a pair of type equalities, one for domains and one for ranges, returns the type equality for the corresponding function types.
        val func<'domain1, 'range1, 'domain2, 'range2> :
            Teq<'domain1, 'domain2> -> Teq<'range1, 'range2> -> Teq<'domain1 -> 'range1, 'domain2 -> 'range2>

        /// Given a type equality between two types 'fst1 and 'fst2, returns the type equality
        /// on the pair types ('fst1 * 'snd) and ('fst2 * 'snd), for any arbitrary 'snd.
        val fst<'fst1, 'fst2, 'snd> : Teq<'fst1, 'fst2> -> Teq<'fst1 * 'snd, 'fst2 * 'snd>

        /// Given a type equality between two types 'snd1 and 'snd2, returns the type equality
        /// on the pair types ('fst * 'snd1) and ('fst * 'snd2), for any arbitrary 'fst.
        val snd<'snd1, 'snd2, 'fst> : Teq<'snd1, 'snd2> -> Teq<'fst * 'snd1, 'fst * 'snd2>

        /// Given a pair of type equalities, one for the first element of a pair and one for the second element of a pair,
        /// returns the type equality for the corresponding pair types.
        val pair<'fst1, 'snd1, 'fst2, 'snd2> :
            Teq<'fst1, 'fst2> -> Teq<'snd1, 'snd2> -> Teq<'fst1 * 'snd1, 'fst2 * 'snd2>
"""

[<Test>]
let ``intrinsic type extension member signature, 413`` () =
    formatSourceString
        true
        """namespace ExtensionParts

type T =
    new: unit -> T

type T with
    member Foo: int
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
namespace ExtensionParts

type T =
    new: unit -> T

type T with
    member Foo: int
"""

[<Test>]
let ``comment above static member, 680`` () =
    formatSourceString
        true
        """
namespace Fantomas

open Fantomas.FormatConfig
open Fantomas.SourceOrigin
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync : fileName:string * source:SourceOrigin * parsingOptions: FSharpParsingOptions * checker:FSharpChecker -> Async<(ParsedInput * string list) array>
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Fantomas

open Fantomas.FormatConfig
open Fantomas.SourceOrigin
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync:
        fileName: string * source: SourceOrigin * parsingOptions: FSharpParsingOptions * checker: FSharpChecker ->
            Async<(ParsedInput * string list) array>
"""

[<Test>]
let ``type restrictions, 797`` () =
    formatSourceString
        true
        """namespace Foo

type internal Foo2 =
  abstract member Bar<'k> : unit -> unit when 'k : comparison
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type internal Foo2 =
    abstract member Bar<'k> : unit -> unit when 'k: comparison
"""

[<Test>]
let ``operator with constraint`` () =
    formatSourceString
        true
        """namespace Bar
    val inline (.+.) : x : ^a Foo -> y : ^b Foo -> ^c Foo when (^a or ^b) : (static member (+) : ^a * ^b -> ^c)
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace Bar

val inline (.+.) : x : ^a Foo -> y : ^b Foo -> ^c Foo when (^a or ^b) : (static member (+) : ^a * ^b -> ^c)
"""

[<Test>]
let ``operator with named constraint`` () =
    formatSourceString
        true
        """namespace Bar
    val inline (.+.) : x : ^a Foo -> y : ^b Foo -> z: ^c Foo when (^a or ^b) : (static member (+) : ^a * ^b -> ^c)
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace Bar

val inline (.+.) : x : ^a Foo -> y : ^b Foo -> z : ^c Foo when (^a or ^b) : (static member (+) : ^a * ^b -> ^c)
"""

[<Test>]
let ``preserve abstract keyword`` () =
    formatSourceString
        true
        """namespace Foo

type internal Blah =
  abstract Baz : unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type internal Blah =
    abstract Baz: unit
"""

[<Test>]
let ``internal keyword before short record type, 830`` () =
    formatSourceString
        true
        """namespace Bar
type 'a Baz =
    internal {
        Value : 'a
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Bar

type 'a Baz = internal { Value: 'a }
"""

[<Test>]
let ``internal keyword before long record type`` () =
    formatSourceString
        true
        """namespace Bar

    type A = internal { ALongIdentifier: string; YetAnotherLongIdentifier: bool }"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Bar

type A =
    internal
        { ALongIdentifier: string
          YetAnotherLongIdentifier: bool }
"""

[<Test>]
let ``multiple constraints on function declaration, 886`` () =
    formatSourceString
        true
        """namespace Blah

module Foo =
    val inline sum : ('a -> ^value) -> 'a Foo -> ^value
        when ^value : (static member (+) : ^value * ^value -> ^value) and ^value : (static member Zero : ^value)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Blah

module Foo =
    val inline sum:
        ('a -> ^value) -> 'a Foo -> ^value
            when ^value: (static member (+): ^value * ^value -> ^value) and ^value: (static member Zero: ^value)
"""

[<Test>]
let ``preserve with get property, 945`` () =
    formatSourceString
        true
        """
namespace B
type Foo =
    | Bar of int
    member Item : unit -> int with get
"""
        { config with
            SpaceBeforeColon = true
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
namespace B

type Foo =
    | Bar of int
    member Item : unit -> int with get
"""

[<Test>]
let ``preserve with set property`` () =
    formatSourceString
        true
        """
namespace B
type Foo =
    member Item : int -> unit with set
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace B

type Foo =
    member Item : int -> unit with set
"""

[<Test>]
let ``preserve with get,set`` () =
    formatSourceString
        true
        """
namespace B

type Foo =
    member Item : int with get,  set
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace B

type Foo =
    member Item : int with get, set
"""

[<Test>]
let ``with set after constraint`` () =
    formatSourceString
        true
        """
namespace B

type Foo =
    member Item : 't -> unit when 't   :   comparison   with set
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace B

type Foo =
    member Item : 't -> unit when 't : comparison with set
"""

[<Test>]
let ``preserve abstract member in type, 944`` () =
    formatSourceString
        true
        """
namespace Baz

type Foo =
    abstract member Bar : Type
    abstract Bar2 : Type
    member Bar3 : Type
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace Baz

type Foo =
    abstract member Bar : Type
    abstract Bar2 : Type
    member Bar3 : Type
"""

[<Test>]
let ``comment before union case, 965`` () =
    formatSourceString
        true
        """namespace Blah

/// Comment
type Foo =
/// Another
    | Foo of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Blah

/// Comment
type Foo =
    /// Another
    | Foo of int
"""

[<Test>]
let ``don't add additional newline before subsequent val, 1029`` () =
    formatSourceString
        true
        """
module Some_module

type foo = bool

val bar : bool
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Some_module

type foo = bool

val bar: bool
"""

[<Test>]
let ``don't add duplicate parentheses for TypeAbbrev, 1057`` () =
    formatSourceString
        false
        """
type AB = A -> B list * C -> D
type AB = A -> (B list * C -> D)
type AB = A -> ((B list * C -> D))

type AB = A -> (C -> D)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type AB = A -> B list * C -> D
type AB = A -> (B list * C -> D)
type AB = A -> ((B list * C -> D))

type AB = A -> (C -> D)
"""

[<Test>]
let ``don't add duplicate parentheses for TypeAbbrev in signature file`` () =
    formatSourceString
        true
        """
namespace Foo

type AB = A -> (B list * C -> D)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type AB = A -> (B list * C -> D)
"""

[<Test>]
let ``don't add extra new line between attribute and namespace, 1097`` () =
    formatSourceString
        true
        """
namespace Meh

[<StringEnum>]
[<RequireQualifiedAccess>]
type PayableFilters = | [<CompiledName "statusSelector">] Status
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Meh

[<StringEnum>]
[<RequireQualifiedAccess>]
type PayableFilters = | [<CompiledName "statusSelector">] Status
"""

[<Test>]
let ``don't add extra new line between nested modules, 1105`` () =
    formatSourceString
        true
        """
module Example

module Foo =
    module Bar =
        type t = bool
        val lol: unit -> bool

    type t = int
    val lmao: unit -> bool

module Foo2 =
    module Bar =
        type t = bool

        val lol: unit -> bool

    type t = int

    val lmao: unit -> bool
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Example

module Foo =
    module Bar =
        type t = bool
        val lol: unit -> bool

    type t = int
    val lmao: unit -> bool

module Foo2 =
    module Bar =
        type t = bool

        val lol: unit -> bool

    type t = int

    val lmao: unit -> bool
"""

[<Test>]
let ``don't add extra new line before attribute of type, 1116`` () =
    formatSourceString
        true
        """
module Test

type t1 = bool

[<SomeAttribute>]
type t2 = bool

val foo : bool
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Test

type t1 = bool

[<SomeAttribute>]
type t2 = bool

val foo: bool
"""

[<Test>]
let ``don't add extra new line before attribute of type, only types`` () =
    formatSourceString
        true
        """
module Test

type t1 = bool

[<SomeAttribute>]
type t2 = bool
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Test

type t1 = bool

[<SomeAttribute>]
type t2 = bool
"""

[<Test>]
let ``line comments before access modifier of multiline record type`` () =
    formatSourceString
        true
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
            Barry: string
        }
"""
        { config with MaxRecordWidth = 10 }
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        { Foo: int
          Barry: string }
"""

[<Test>]
let ``line comments before access modifier of single line record type`` () =
    formatSourceString
        true
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Meh : TimeSpan
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        { Meh: TimeSpan }
"""

[<Test>]
let ``format long val return type multiline, 1181`` () =
    formatSourceString
        true
        """
namespace TypeEquality

[<RequireQualifiedAccess>]
module Teq =

    [<RequireQualifiedAccess>]
    module Cong =

        val domainOf<'domain1, 'domain2, 'range1, 'range2> : Teq<'domain1 -> 'range1, 'domain2 -> 'range2>
             -> Teq<'domain1, 'domain2>
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
namespace TypeEquality

[<RequireQualifiedAccess>]
module Teq =

    [<RequireQualifiedAccess>]
    module Cong =

        val domainOf<'domain1, 'domain2, 'range1, 'range2> :
            Teq<'domain1 -> 'range1, 'domain2 -> 'range2> -> Teq<'domain1, 'domain2>
"""

[<Test>]
let ``inline type definition member, 1399`` () =
    formatSourceString
        true
        """
namespace Baz

[<Sealed>]
type Foo =
    member inline Return : 'a -> Baz<'a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

[<Sealed>]
type Foo =
    member inline Return: 'a -> Baz<'a>
"""

[<Test>]
let ``inline private type definition member`` () =
    formatSourceString
        true
        """
namespace Baz

[<Sealed>]
type Foo =
    member inline private Return : 'a -> Baz<'a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

[<Sealed>]
type Foo =
    member inline private Return: 'a -> Baz<'a>
"""

[<Test>]
let ``surround return type annotations with white space`` () =
    formatSourceString
        true
        """
namespace Foo

val blah:int

type C =
    member P1 : int * string
    /// def
    member P2 : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

val blah: int

type C =
    member P1: int * string
    /// def
    member P2: int
"""

[<Test>]
let ``long val signature, 1515`` () =
    formatSourceString
        true
        """
namespace Bug

val create : something_really_long : unit -> another_really_long_thing : unit -> and_another_to_make_the_line_long_enough : unit -> unit
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
namespace Bug

val create:
  something_really_long: unit ->
  another_really_long_thing: unit ->
  and_another_to_make_the_line_long_enough: unit ->
    unit
"""

[<Test>]
let ``print trivia before exception`` () =
    formatSourceString
        true
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// The configuration of the compiler (TcConfig and TcConfigBuilder)
module internal FSharp.Compiler.CompilerConfig

open System
open Internal.Utilities
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.AbstractIL.ILPdbWriter
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Features
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
exception FileNameNotResolved of string (*description of searched locations*)  * string * range (*filename*)

exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// The configuration of the compiler (TcConfig and TcConfigBuilder)
module internal FSharp.Compiler.CompilerConfig

open System
open Internal.Utilities
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.AbstractIL.ILPdbWriter
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Features
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

exception FileNameNotResolved of string (*description of searched locations*) * string * range (*filename*)

exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
"""

[<Test>]
let ``comment above first DU case`` () =
    formatSourceString
        true
        """
namespace Baz

type 'a Bar =
    ///
    | Foo
    ///
    | Quux
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

type 'a Bar =
    ///
    | Foo
    ///
    | Quux
"""

[<Test>]
let ``comment between attribute and val, 1561`` () =
    formatSourceString
        true
        """
namespace Baz

module Bar =

    [<Obsolete "">]
    ////
    val f : unit -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

module Bar =

    [<Obsolete "">]
    ////
    val f: unit -> unit
"""

[<Test>]
let ``comment between recursive type, 1562`` () =
    formatSourceString
        true
        """
namespace Baz

type Foo = | Foo of int

///
and [<RequireQualifiedAccess>] Bar<'a> =
    | Bar of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

type Foo = Foo of int

///
and [<RequireQualifiedAccess>] Bar<'a> = Bar of int
"""

[<Test>]
let ``comments between multiple recursive types`` () =
    formatSourceString
        true
        """
namespace Baz

type Foo = | Foo of int

/// barry
and Bar = | Bar of string

/// mehhy
and Meh = | Meh of DateTime
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

type Foo = Foo of int

/// barry
and Bar = Bar of string

/// mehhy
and Meh = Meh of DateTime
"""

[<Test>]
let ``val inline internal, 1590`` () =
    formatSourceString
        true
        """
module internal FSharp.Compiler.TypedTreePickle

/// Deserialize a tuple
val inline  internal u_tup4 : unpickler<'T2> -> unpickler<'T3> -> unpickler<'T4> -> unpickler<'T5> -> unpickler<'T2 * 'T3 * 'T4 * 'T5>
"""
        config
    |> prepend newline
    |> should
        equal
        """
module internal FSharp.Compiler.TypedTreePickle

/// Deserialize a tuple
val inline internal u_tup4:
    unpickler<'T2> -> unpickler<'T3> -> unpickler<'T4> -> unpickler<'T5> -> unpickler<'T2 * 'T3 * 'T4 * 'T5>
"""

[<Test>]
let ``comments after indents in multiline type function signature, 1287`` () =
    formatSourceString
        true
        """
namespace Test

module OrderProcessing =
  type ValidateOrder =
    CheckProductCodeExists    // dependency
      -> CheckAddressExists   // dependency
      -> UnvalidatedOrder     // input
      -> Result<ValidatedOrder,ValidationError>  // output (Result b/c one of deps returns a Result)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
namespace Test

module OrderProcessing =
    type ValidateOrder =
        CheckProductCodeExists // dependency
            -> CheckAddressExists // dependency
            -> UnvalidatedOrder // input
            -> Result<ValidatedOrder, ValidationError> // output (Result b/c one of deps returns a Result)
"""

[<Test>]
let ``take newline trivia between recursive types into account, 1605`` () =
    formatSourceString
        true
        """
namespace Test

///
type Foo =
    ///
    | Bar

///
and internal Hi<'a> =
    ///
    abstract Apply<'b> : Foo -> 'b


///
and [<CustomEquality>] Bang =
    internal
        {
            LongNameBarBarBarBarBarBarBar: int
        }
        ///
        override GetHashCode : unit -> int
"""
        { config with
            MultilineBlockBracketsOnSameColumn = true
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
namespace Test

///
type Foo =
    ///
    | Bar

///
and internal Hi<'a> =
    ///
    abstract Apply<'b> : Foo -> 'b


///
and [<CustomEquality>] Bang =
    internal
        {
            LongNameBarBarBarBarBarBarBar: int
        }
    ///
    override GetHashCode: unit -> int
"""

[<Test>]
let ``xml comment before SynTypeDefnSimpleRepr.Union should keep bar, 1563`` () =
    formatSourceString
        true
        """
namespace Baz

type Foo =
    /// Hi!
    | Bar of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Baz

type Foo =
    /// Hi!
    | Bar of int
"""

[<Test>]
let ``long multiline prefix type name should be indented far enough, 1687`` () =
    formatSourceString
        true
        """
namespace Foo

type Bar =
    member Hello : thing : XLongLongLongLongLongLongLongLong<bool -> 'a, bool -> 'b, bool -> 'c, bool -> 'd, bool -> ('e -> 'f) -> 'g, ('h -> 'i) -> 'j> * item : int list -> LongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLong
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type Bar =
    member Hello :
        thing :
            XLongLongLongLongLongLongLongLong<
                bool -> 'a,
                bool -> 'b,
                bool -> 'c,
                bool -> 'd,
                bool -> ('e -> 'f) -> 'g,
                ('h -> 'i) -> 'j
            > *
        item : int list ->
            LongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLong
"""

[<Test>]
let ``a record type with accessibility modifier and members`` () =
    formatSourceString
        true
        """
namespace Thing

type Foo =
    private
        {
            Bar : int
            Qux : string
        }
    static member Baz : int
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
namespace Thing

type Foo =
    private
        { Bar: int
          Qux: string }
    static member Baz: int
"""

[<Test>]
let ``mod name in val, 1960`` () =
    formatSourceString
        true
        """
module X

val ``mod``: t -> t -> t
"""
        config
    |> prepend newline
    |> should
        equal
        """
module X

val ``mod``: t -> t -> t
"""

[<Test>]
let ``literals in signatures, 1953`` () =
    formatSourceString
        true
        """
namespace Foo

[<Literal>]
val parenGet: string = ".()"
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

[<Literal>]
val parenGet: string = ".()"
"""

[<Test>]
let ``literals in signatures, sig member`` () =
    formatSourceString
        true
        """
namespace Meh

type FooBar =
    [<Literal>]
    abstract member parenGet : string = ".()"
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Meh

type FooBar =
    [<Literal>]
    abstract member parenGet: string = ".()"
"""

[<Test>]
let ``trivia before exception with attributes, 1974`` () =
    formatSourceString
        true
        """
module internal FSharp.Compiler.ParseHelpers

open FSharp.Compiler.AbstractIL.IL


/// The error raised by the parse_error_rich function, which is called by the parser engine
[<NoEquality; NoComparison>]
exception SyntaxError of obj * range: range
"""
        config
    |> prepend newline
    |> should
        equal
        """
module internal FSharp.Compiler.ParseHelpers

open FSharp.Compiler.AbstractIL.IL


/// The error raised by the parse_error_rich function, which is called by the parser engine
[<NoEquality; NoComparison>]
exception SyntaxError of obj * range: range
"""

[<Test>]
let ``multiline tupled signature`` () =
    formatSourceString
        true
        """
    namespace Oslo
    type Meh =
        member ResolveDependencies:
            scriptDirectory: string
            * scriptName: string
            * scriptExt: string
            * timeout: int ->
            obj
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory: string *
        scriptName: string *
        scriptExt: string *
        timeout: int ->
            obj
"""

[<Test>]
let ``add extra indent when the next parameter is a tuple`` () =
    formatSourceString
        true
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory: string * scriptName: string ->
        scriptName: string
        * scriptExt: string
        * timeout: int ->
        obj
"""
        { config with MaxLineLength = 5 }
    |> prepend newline
    |> should
        equal
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory:
            string *
        scriptName:
            string ->
            scriptName:
                string *
            scriptExt:
                string *
            timeout:
                int ->
                obj
"""

[<Test>]
let ``mixed curried and tupled arguments`` () =
    formatSourceString
        true
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory: string * scriptName: string ->
        scriptName: string ->
        obj
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory:
            string *
        scriptName: string ->
            scriptName: string ->
                obj
"""

[<Test>]
let ``unindent correctly after type signature`` () =
    formatSourceString
        true
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory: string * scriptName: string ->
        scriptName: string ->
        obj

val x : int
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        scriptDirectory:
            string *
        scriptName: string ->
            scriptName: string ->
                obj

val x: int
"""

[<Test>]
let ``only indent after tuple in non last position`` () =
    formatSourceString
        true
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        criptName: string -> foo: string -> scriptDirectory: string * scriptName: string -> // after a tuple, mixed needs an indent
                                                                                            scriptName: string -> obj
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
namespace Oslo

type Meh =
    member ResolveDependencies:
        criptName: string ->
        foo: string ->
        scriptDirectory:
            string *
        scriptName: string -> // after a tuple, mixed needs an indent
            scriptName: string ->
                obj
"""

[<Test>]
let ``tupled function signature`` () =
    formatSourceString
        true
        """
namespace StyleGuide

val SampleTupledFunction:
    arg1: string * arg2: string * arg3: int * arg4: int -> int list
"""
        { config with MaxLineLength = 15 }
    |> prepend newline
    |> should
        equal
        """
namespace StyleGuide

val SampleTupledFunction:
    arg1:
        string *
    arg2:
        string *
    arg3: int *
    arg4: int ->
        int list
"""

[<Test>]
let ``optional parameter in static type member, 2144`` () =
    formatSourceString
        true
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Control
    
    [<Sealed>]
    [<CompiledName("FSharpAsync")>]
    type Async =
        static member AwaitEvent: event:IEvent<'Del,'T> * ?cancelAction : (unit -> unit) -> Async<'T> when 'Del : delegate<'T,unit> and 'Del :> System.Delegate 
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Control

[<Sealed>]
[<CompiledName("FSharpAsync")>]
type Async =
    static member AwaitEvent:
        event: IEvent<'Del, 'T> * ?cancelAction: (unit -> unit) -> Async<'T>
            when 'Del: delegate<'T, unit> and 'Del :> System.Delegate
"""

[<Test>]
let ``multi-constrained SRTP functions, 2230`` () =
    formatSourceString
        true
        """
/// Throws <c>ArgumentException</c>
/// </example>
[<CompiledName("Average")>]
val inline average   : array:^T[] -> ^T   
                            when ^T : (static member ( + ) : ^T * ^T -> ^T) 
                            and  ^T : (static member DivideByInt : ^T*int -> ^T) 
                            and  ^T : (static member Zero : ^T)
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Throws <c>ArgumentException</c>
/// </example>
[<CompiledName("Average")>]
val inline average:
    array: ^T[] -> ^T
        when ^T: (static member (+): ^T * ^T -> ^T)
        and ^T: (static member DivideByInt: ^T * int -> ^T)
        and ^T: (static member Zero: ^T)
"""

[<Test>]
let ``long curried value with constraints`` () =
    formatSourceString
        true
        """
[<CompiledName("Average")>]
val inline average: array: ^T[] -> array: ^T[] -> array: ^T[] -> array: ^T[] -> array: ^T[] -> array: ^T[] -> array: ^T[] -> array: ^T[] -> array: ^T[] -> ^T
    when ^T: (static member (+): ^T * ^T -> ^T)
    and ^T: (static member DivideByInt: ^T * int -> ^T)
    and ^T: (static member Zero: ^T)
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<CompiledName("Average")>]
val inline average:
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
    array: ^T[] ->
        ^T
        when ^T: (static member (+): ^T * ^T -> ^T)
        and ^T: (static member DivideByInt: ^T * int -> ^T)
        and ^T: (static member Zero: ^T)
"""

[<Test>]
let ``long tupled value with constraints`` () =
    formatSourceString
        true
        """
[<CompiledName("Average")>]
val inline average: array: ^T[] * array: ^T[] * array: ^T[] * array: ^T[] * array: ^T[] * array: ^T[] * array: ^T[] * array: ^T[] * array: ^T[] -> ^T
    when ^T: (static member (+): ^T * ^T -> ^T)
    and ^T: (static member DivideByInt: ^T * int -> ^T)
    and ^T: (static member Zero: ^T)
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<CompiledName("Average")>]
val inline average:
    array: ^T[] *
    array: ^T[] *
    array: ^T[] *
    array: ^T[] *
    array: ^T[] *
    array: ^T[] *
    array: ^T[] *
    array: ^T[] *
    array: ^T[] ->
        ^T
        when ^T: (static member (+): ^T * ^T -> ^T)
        and ^T: (static member DivideByInt: ^T * int -> ^T)
        and ^T: (static member Zero: ^T)
"""

[<Test>]
let ``long mixed curried and tuple value with constraints`` () =
    formatSourceString
        true
        """
[<CompiledName("Average")>]
val inline average: array: ^T[] * array: ^T[] * array: ^T[] -> array: ^T[] * array: ^T[] * array: ^T[] -> array: ^T[] * array: ^T[] * array: ^T[] -> ^T
    when ^T: (static member (+): ^T * ^T -> ^T)
    and ^T: (static member DivideByInt: ^T * int -> ^T)
    and ^T: (static member Zero: ^T)
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<CompiledName("Average")>]
val inline average:
    array: ^T[] * array: ^T[] * array: ^T[] ->
        array: ^T[] * array: ^T[] * array: ^T[] ->
            array: ^T[] * array: ^T[] * array: ^T[] ->
                ^T
        when ^T: (static member (+): ^T * ^T -> ^T)
        and ^T: (static member DivideByInt: ^T * int -> ^T)
        and ^T: (static member Zero: ^T)
"""

[<Test>]
let ``blank line under struct type name`` () =
    formatSourceString
        true
        """
[<Struct>]
type ILVersionInfo =

    val Major: uint16
    val Minor: uint16
    val Build: uint16
    val Revision: uint16
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Struct>]
type ILVersionInfo =

    val Major: uint16
    val Minor: uint16
    val Build: uint16
    val Revision: uint16
"""

[<Test>]
let ``type augmentations with trivia between members`` () =
    formatSourceString
        true
        """
    type DiagnosticsLogger with

        member ErrorR: exn: exn -> unit

        member Warning: exn: exn -> unit

        member Error: exn: exn -> 'T

        member SimulateError: diagnostic: PhasedDiagnostic -> 'T

        member ErrorRecovery: exn: exn -> m: range -> unit

        member StopProcessingRecovery: exn: exn -> m: range -> unit

        member ErrorRecoveryNoRange: exn: exn -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
type DiagnosticsLogger with

    member ErrorR: exn: exn -> unit

    member Warning: exn: exn -> unit

    member Error: exn: exn -> 'T

    member SimulateError: diagnostic: PhasedDiagnostic -> 'T

    member ErrorRecovery: exn: exn -> m: range -> unit

    member StopProcessingRecovery: exn: exn -> m: range -> unit

    member ErrorRecoveryNoRange: exn: exn -> unit
"""

[<Test>]
let ``trivia between xml comment and type keyword, 2143`` () =
    formatSourceString
        true
        """
/// Represents a line number when using zero-based line counting (used by Visual Studio)
// #if CHECK_LINE0_TYPES

// #else
type Line0 = int
// #endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Represents a line number when using zero-based line counting (used by Visual Studio)
// #if CHECK_LINE0_TYPES

// #else
type Line0 = int
// #endif
"""

[<Test>]
let ``add empty line after module, 2502`` () =
    formatSourceString
        true
        """
module A
open System
val a: DateTime
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A

open System
val a: DateTime
"""

[<Test>]
let ``add empty line after namespace`` () =
    formatSourceString
        true
        """
namespace A
exception MyFSharpError1 of string
exception MyFSharpError2 of string * int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace A

exception MyFSharpError1 of string
exception MyFSharpError2 of string * int
"""

[<Test>]
let ``long named parameter type in signature`` () =
    formatSourceString
        true
        """
val a: b: veryLoooooooooooooooooooooooooooooooooooooooooooooongTypeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
"""
        config
    |> prepend newline
    |> should
        equal
        """
val a:
    b:
        veryLoooooooooooooooooooooooooooooooooooooooooooooongTypeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
"""

[<Test>]
let ``internal constructor in signature`` () =
    formatSourceString
        true
        """
/// IL Method definitions.
[<NoComparison; NoEquality>]
type ILMethodDef =

    /// Functional creation of a value, with delayed reading of some elements via a metadata index
    internal new:
        name: string *
        attributes: MethodAttributes *
        implAttributes: MethodImplAttributes *
        callingConv: ILCallingConv *
        parameters: ILParameters *
        ret: ILReturn *
        body: Lazy<MethodBody> *
        isEntryPoint: bool *
        genericParams: ILGenericParameterDefs *
        securityDeclsStored: ILSecurityDeclsStored *
        customAttrsStored: ILAttributesStored *
        metadataIndex: int32 ->
            ILMethodDef
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// IL Method definitions.
[<NoComparison; NoEquality>]
type ILMethodDef =

    /// Functional creation of a value, with delayed reading of some elements via a metadata index
    internal new:
        name: string *
        attributes: MethodAttributes *
        implAttributes: MethodImplAttributes *
        callingConv: ILCallingConv *
        parameters: ILParameters *
        ret: ILReturn *
        body: Lazy<MethodBody> *
        isEntryPoint: bool *
        genericParams: ILGenericParameterDefs *
        securityDeclsStored: ILSecurityDeclsStored *
        customAttrsStored: ILAttributesStored *
        metadataIndex: int32 ->
            ILMethodDef
"""
