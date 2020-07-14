module Fantomas.Tests.SignatureTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since "(string * string) list" is converted to "string * string list"
[<Test>]
let ``should keep the (string * string) list type signature in records``() =
    formatSourceString false """type MSBuildParams = 
    { Targets : string list
      Properties : (string * string) list
      MaxCpuCount : int option option
      ToolsVersion : string option
      Verbosity : MSBuildVerbosity option
      FileLoggers : MSBuildFileLoggerConfig list option }

    """ config
    |> should equal """type MSBuildParams =
    { Targets: string list
      Properties: (string * string) list
      MaxCpuCount: int option option
      ToolsVersion: string option
      Verbosity: MSBuildVerbosity option
      FileLoggers: MSBuildFileLoggerConfig list option }
"""

[<Test>]
let ``should keep the (string * string) list type signature in functions``() =
    shouldNotChangeAfterFormat """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> (string * string) list) projects =
    doingsomstuff
"""


[<Test>]
let ``should keep the string * string list type signature in functions``() =
    shouldNotChangeAfterFormat """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: (string -> string) * string list) projects =
    doingsomstuff
"""


[<Test>]
let ``should not add parens in signature``() =
    formatSourceString false """type Route = 
    { Verb : string
      Path : string
      Handler : Map<string, string> -> HttpListenerContext -> string }
    override x.ToString() = sprintf "%s %s" x.Verb x.Path

    """ { config with MaxFunctionBindingWidth = 120 }
    |> should equal """type Route =
    { Verb: string
      Path: string
      Handler: Map<string, string> -> HttpListenerContext -> string }
    override x.ToString() = sprintf "%s %s" x.Verb x.Path
"""

[<Test>]
let ``should keep the string * string * string option type signature``() =
    formatSourceString false """type DGML = 
    | Node of string
    | Link of string * string * (string option)

    """ config
    |> should equal """type DGML =
    | Node of string
    | Link of string * string * (string option)
"""

[<Test>]
let ``should keep the (string option * Node) list type signature``() =
    formatSourceString false """type Node = 
    { Name : string;
      NextNodes : (string option * Node) list }

    """ { config with SemicolonAtEndOfLine = true }
    |> should equal """type Node =
    { Name: string;
      NextNodes: (string option * Node) list }
"""

[<Test>]
let ``should keep parentheses on the left of type signatures``() =
    formatSourceString false """type IA =
    abstract F: (unit -> Option<'T>) -> Option<'T>

type A () =
    interface IA with
        member x.F (f: unit -> _) = f ()
    """ config
    |> should equal """type IA =
    abstract F: (unit -> Option<'T>) -> Option<'T>

type A() =
    interface IA with
        member x.F(f: unit -> _) = f ()
"""

[<Test>]
let ``should not add parentheses around bare tuples``() =
    formatSourceString true """
namespace TupleType
type C =
    member P1 : int * string
    /// def
    member P2 : int
"""  config
    |> prepend newline
    |> should equal """
namespace TupleType

type C =
    member P1: int * string
    /// def
    member P2: int
"""

[<Test>]
let ``should keep global constraints in type signature``() =
    formatSourceString true """
module Tainted
val GetHashCodeTainted : (Tainted<'T> -> int) when 'T : equality
"""  config
    |> prepend newline
    |> should equal """
module Tainted

val GetHashCodeTainted: (Tainted<'T> -> int) when 'T: equality
"""

[<Test>]
let ``should keep access modifiers in signatures seperated``() =
    formatSourceString true """
module Test
type Test =
    static member internal FormatAroundCursorAsync : fileName:string -> unit
"""  config
    |> prepend newline
    |> should equal """
module Test

type Test =
    static member internal FormatAroundCursorAsync: fileName:string -> unit
"""

[<Test>]
let ``comment should stay above type`` () =
    formatSourceString true """namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>
"""  config
    |> prepend newline
    |> should equal """
namespace TypeEquality

/// A type for witnessing type equality between 'a and 'b
type Teq<'a, 'b>
"""

[<Test>]
let ``comment before namespace should be preserved`` () =
    formatSourceString true """
// some comment
namespace TypeEquality

type Teq<'a, 'b>
"""  config
    |> prepend newline
    |> should equal """
// some comment
namespace TypeEquality

type Teq<'a, 'b>
"""

[<Test>]
let ``generic val in nested module should keep generic type parameters`` () =
    formatSourceString true """namespace TypeEquality

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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString true """namespace TypeEquality

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
    val symmetry : Teq<'a, 'b> -> Teq<'b, 'a>

    /// Let's compose two type-equalities: a = b && b = c => a = c
    val transitivity : Teq<'a, 'b> -> Teq<'b, 'c> -> Teq<'a, 'c>

    /// Converts an 'a to a 'b
    val cast : Teq<'a, 'b> -> ('a -> 'b)

    /// Converts an 'a to a 'b
    /// Alias for cast
    val castTo : Teq<'a, 'b> -> ('a -> 'b)

    /// Converts a 'b to an 'a
    /// Equivalent to symmetry >> cast, but more efficient
    val castFrom : Teq<'a, 'b> -> ('b -> 'a)

    /// Utility function to map an object of one type using a mapping function
    /// for a different type when we have a type equality between the two types
    val mapAs : Teq<'a, 'b> -> ('b -> 'b) -> 'a -> 'a

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
"""  config
    |> prepend newline
    |> should equal """
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
        val domainOf<'domain1, 'domain2, 'range1, 'range2> : Teq<'domain1 -> 'range1, 'domain2 -> 'range2>
             -> Teq<'domain1, 'domain2>

        /// Given a type equality between two types 'range1 and 'range2, returns the type equality
        /// on the function types ('domain -> 'range1) and ('domain -> 'range2), for any arbitrary 'domain.
        val range<'domain, 'range1, 'range2> : Teq<'range1, 'range2> -> Teq<'domain -> 'range1, 'domain -> 'range2>

        /// Given a type equality between two function types, returns the type equality on their corresponding ranges.
        val rangeOf<'domain1, 'domain2, 'range1, 'range2> : Teq<'domain1 -> 'range1, 'domain2 -> 'range2>
             -> Teq<'range1, 'range2>

        /// Given a pair of type equalities, one for domains and one for ranges, returns the type equality for the corresponding function types.
        val func<'domain1, 'range1, 'domain2, 'range2> : Teq<'domain1, 'domain2>
             -> Teq<'range1, 'range2> -> Teq<'domain1 -> 'range1, 'domain2 -> 'range2>

        /// Given a type equality between two types 'fst1 and 'fst2, returns the type equality
        /// on the pair types ('fst1 * 'snd) and ('fst2 * 'snd), for any arbitrary 'snd.
        val fst<'fst1, 'fst2, 'snd> : Teq<'fst1, 'fst2> -> Teq<'fst1 * 'snd, 'fst2 * 'snd>

        /// Given a type equality between two types 'snd1 and 'snd2, returns the type equality
        /// on the pair types ('fst * 'snd1) and ('fst * 'snd2), for any arbitrary 'fst.
        val snd<'snd1, 'snd2, 'fst> : Teq<'snd1, 'snd2> -> Teq<'fst * 'snd1, 'fst * 'snd2>

        /// Given a pair of type equalities, one for the first element of a pair and one for the second element of a pair,
        /// returns the type equality for the corresponding pair types.
        val pair<'fst1, 'snd1, 'fst2, 'snd2> : Teq<'fst1, 'fst2>
             -> Teq<'snd1, 'snd2> -> Teq<'fst1 * 'snd1, 'fst2 * 'snd2>
"""

[<Test>]
let ``intrinsic type extension member signature, 413`` () =
    formatSourceString true """namespace ExtensionParts

type T =
    new: unit -> T

type T with
    member Foo: int
"""  config
    |> prepend newline
    |> should equal """
namespace ExtensionParts

type T =
    new: unit -> T

type T with
    member Foo: int
"""

[<Test>]
let ``comment above static member, 680`` () =
    formatSourceString true """
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
"""  config
    |> prepend newline
    |> should equal """
namespace Fantomas

open Fantomas.FormatConfig
open Fantomas.SourceOrigin
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync: fileName:string * source:SourceOrigin * parsingOptions:FSharpParsingOptions * checker:FSharpChecker
         -> Async<(ParsedInput * string list) array>
"""

[<Test>]
let ``type restrictions, 797`` () =
    formatSourceString true """namespace Foo

type internal Foo2 =
  abstract member Bar<'k> : unit -> unit when 'k : comparison
"""  config
    |> prepend newline
    |> should equal """
namespace Foo

type internal Foo2 =
    abstract member Bar<'k> : unit -> unit when 'k: comparison
"""

[<Test>]
let ``operator with constraint`` () =
    formatSourceString true """namespace Bar
    val inline (.+.) : x : ^a Foo -> y : ^b Foo -> ^c Foo when (^a or ^b) : (static member (+) : ^a * ^b -> ^c)
"""  config
    |> prepend newline
    |> should equal """
namespace Bar

val inline (.+.): x : ^a Foo -> y : ^b Foo -> ^c Foo when (^a or ^b): (static member (+): ^a * ^b -> ^c)
"""

[<Test>]
let ``preserve abstract keyword`` () =
    formatSourceString true """namespace Foo

type internal Blah =
  abstract Baz : unit
"""  config
    |> prepend newline
    |> should equal """
namespace Foo

type internal Blah =
    abstract Baz: unit
"""

[<Test>]
let ``internal keyword before short record type, 830`` () =
    formatSourceString true """namespace Bar
type 'a Baz =
    internal {
        Value : 'a
    }
"""  config
    |> prepend newline
    |> should equal """
namespace Bar

type 'a Baz = internal { Value: 'a }
"""

[<Test>]
let ``internal keyword before long record type`` () =
    formatSourceString true """namespace Bar

    type A = internal { ALongIdentifier: string; YetAnotherLongIdentifier: bool }""" config
    |> prepend newline
    |> should equal """
namespace Bar

type A =
    internal { ALongIdentifier: string
               YetAnotherLongIdentifier: bool }
"""

[<Test>]
let ``multiple constraints on function declaration, 886`` () =
    formatSourceString true """namespace Blah

module Foo =
    val inline sum : ('a -> ^value) -> 'a Foo -> ^value
        when ^value : (static member (+) : ^value * ^value -> ^value) and ^value : (static member Zero : ^value)
"""  config
    |> prepend newline
    |> should equal """
namespace Blah

module Foo =
    val inline sum: ('a -> ^value) -> 'a Foo -> ^value
        when ^value: (static member (+): ^value * ^value -> ^value) and ^value: (static member Zero: ^value)
"""

[<Test>]
let ``preserve with get property, 945`` () =
    formatSourceString true """
namespace B
type Foo =
    | Bar of int
    member Item : unit -> int with get
"""  { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should equal """
namespace B

type Foo =
    | Bar of int
    member Item : unit -> int with get
"""

[<Test>]
let ``preserve with set property`` () =
    formatSourceString true """
namespace B
type Foo =
    member Item : int -> unit with set
"""  { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should equal """
namespace B

type Foo =
    member Item : int -> unit with set
"""

[<Test>]
let ``preserve with get,set`` () =
    formatSourceString true """
namespace B

type Foo =
    member Item : int with get,  set
"""  { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should equal """
namespace B

type Foo =
    member Item : int with get, set
"""

[<Test>]
let ``with set after constraint`` () =
    formatSourceString true """
namespace B

type Foo =
    member Item : 't -> unit when 't   :   comparison   with set
"""  { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should equal """
namespace B

type Foo =
    member Item : 't -> unit when 't : comparison with set
"""

[<Test>]
let ``preserve abstract member in type, 944`` () =
    formatSourceString true """
namespace Baz

type Foo =
    abstract member Bar : Type
    abstract Bar2 : Type
    member Bar3 : Type
"""  { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should equal """
namespace Baz

type Foo =
    abstract member Bar : Type
    abstract Bar2 : Type
    member Bar3 : Type
"""
