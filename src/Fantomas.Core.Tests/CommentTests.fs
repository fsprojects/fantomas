module Fantomas.Core.Tests.CommentTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``should keep sticky-to-the-left comments after nowarn directives`` () =
    formatSourceString false """#nowarn "51" // address-of operator can occur in the code""" config
    |> should
        equal
        """#nowarn "51" // address-of operator can occur in the code
"""

[<Test>]
let ``should keep sticky-to-the-right comments before module definition`` () =
    let source =
        """
// The original idea for this typeprovider is from Ivan Towlson
// some text
module FSharpx.TypeProviders.VectorTypeProvider

let x = 1"""

    formatSourceString false source config
    |> should
        equal
        """// The original idea for this typeprovider is from Ivan Towlson
// some text
module FSharpx.TypeProviders.VectorTypeProvider

let x = 1
"""

[<Test>]
let ``comments on local let bindings`` () =
    formatSourceString
        false
        """
let print_30_permut() =

    /// declare and initialize
    let permutation : int array = Array.init n (fun i -> Console.Write(i+1); i)
    permutation
    """
        config
    |> prepend newline
    |> should
        equal
        """
let print_30_permut () =

    /// declare and initialize
    let permutation: int array =
        Array.init n (fun i ->
            Console.Write(i + 1)
            i)

    permutation
"""

[<Test>]
let ``comments on local let bindings with desugared lambda`` () =
    formatSourceString
        false
        """
let print_30_permut() =

    /// declare and initialize
    let permutation : int array = Array.init n (fun (i,j) -> Console.Write(i+1); i)
    permutation
    """
        config
    |> prepend newline
    |> should
        equal
        """
let print_30_permut () =

    /// declare and initialize
    let permutation: int array =
        Array.init n (fun (i, j) ->
            Console.Write(i + 1)
            i)

    permutation
"""

[<Test>]
let ``comments on let bindings with return type, 2043`` () =
    formatSourceString
        false
        """
let count: int[] // foo
    = [2]
    """
        config
    |> prepend newline
    |> should
        equal
        """
let count: int[] // foo
    =
    [ 2 ]
"""

[<Test>]
let ``comments after SynType_Array in record type, 2043`` () =
    formatSourceString
        false
        """
type Model =
    { Flags: bool[] // foo
      Name: string // bar
      Street: string }
    """
        config
    |> prepend newline
    |> should
        equal
        """
type Model =
    { Flags: bool[] // foo
      Name: string // bar
      Street: string }
"""

[<Test>]
let ``xml documentation`` () =
    formatSourceString
        false
        """
/// <summary>
/// Kill Weight Mud
/// </summary>
///<param name="sidpp">description</param>
///<param name="tvd">xdescription</param>
///<param name="omw">ydescription</param>
let kwm sidpp tvd omw =
    (sidpp / 0.052 / tvd) + omw

/// Kill Weight Mud
let kwm sidpp tvd omw = 1.0"""
        config
    |> prepend newline
    |> should
        equal
        """
/// <summary>
/// Kill Weight Mud
/// </summary>
///<param name="sidpp">description</param>
///<param name="tvd">xdescription</param>
///<param name="omw">ydescription</param>
let kwm sidpp tvd omw = (sidpp / 0.052 / tvd) + omw

/// Kill Weight Mud
let kwm sidpp tvd omw = 1.0
"""

[<Test>]
let ``should preserve comment-only source code`` () =
    formatSourceString
        false
        """(*
  line1
  line2
*)
"""
        config
    |> should
        equal
        """(*
  line1
  line2
*)
"""

[<Test>]
let ``should keep sticky-to-the-right comments`` () =
    formatSourceString
        false
        """
let f() =
    // COMMENT
    x + x
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f () =
    // COMMENT
    x + x
"""

[<Test>]
let ``should keep sticky-to-the-left comments`` () =
    formatSourceString
        false
        """
let f() =
  let x = 1 // COMMENT
  x + x
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f () =
    let x = 1 // COMMENT
    x + x
"""

[<Test>]
let ``should keep well-aligned comments`` () =
    formatSourceString
        false
        """
/// XML COMMENT
// Other comment
let f() =
    // COMMENT A
    let y = 1
    (* COMMENT B *)
    (* COMMENT C *)
    x + x + x

"""
        config
    |> prepend newline
    |> should
        equal
        """
/// XML COMMENT
// Other comment
let f () =
    // COMMENT A
    let y = 1
    (* COMMENT B *)
    (* COMMENT C *)
    x + x + x
"""

[<Test>]
let ``should align mis-aligned comments`` () =
    formatSourceString
        false
        """
   /// XML COMMENT A
     // Other comment
let f() =
      // COMMENT A
    let y = 1
      /// XML COMMENT B
    let z = 1
  // COMMENT B
    x + x + x

"""
        config
    |> prepend newline
    |> should
        equal
        """
/// XML COMMENT A
// Other comment
let f () =
    // COMMENT A
    let y = 1
    /// XML COMMENT B
    let z = 1
    // COMMENT B
    x + x + x
"""

[<Test>]
let ``should indent comments properly`` () =
    formatSourceString
        false
        """
/// Non-local information related to internals of code generation within an assembly
type IlxGenIntraAssemblyInfo =
    { /// A table recording the generated name of the static backing fields for each mutable top level value where
      /// we may need to take the address of that value, e.g. static mutable module-bound values which are structs. These are
      /// only accessible intra-assembly. Across assemblies, taking the address of static mutable module-bound values is not permitted.
      /// The key to the table is the method ref for the property getter for the value, which is a stable name for the Val's
      /// that come from both the signature and the implementation.
      StaticFieldInfo : Dictionary<ILMethodRef, ILFieldSpec> }

"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Non-local information related to internals of code generation within an assembly
type IlxGenIntraAssemblyInfo =
    {
        /// A table recording the generated name of the static backing fields for each mutable top level value where
        /// we may need to take the address of that value, e.g. static mutable module-bound values which are structs. These are
        /// only accessible intra-assembly. Across assemblies, taking the address of static mutable module-bound values is not permitted.
        /// The key to the table is the method ref for the property getter for the value, which is a stable name for the Val's
        /// that come from both the signature and the implementation.
        StaticFieldInfo: Dictionary<ILMethodRef, ILFieldSpec>
    }
"""

[<Test>]
let ``should add comment after { as part of property assignment`` () =
    formatSourceString
        false
        """
let a =
    { // foo
    // bar
    B = 7 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    { // foo
      // bar
      B = 7 }
"""

[<Test>]
let ``comment alignment above record field`` () =
    formatSourceString
        false
        """
let a =
    { c = 4
      // foo
      // bar
      B = 7 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    { c = 4
      // foo
      // bar
      B = 7 }
"""

[<Test>]
let ``comment alignment above record field, fsharp_space_around_delimiter = false`` () =
    formatSourceString
        false
        """
let a =
    { c = 4
      // foo
      // bar
      B = 7 }
"""
        { config with SpaceAroundDelimiter = false }
    |> prepend newline
    |> should
        equal
        """
let a =
    {c = 4
     // foo
     // bar
     B = 7}
"""

[<Test>]
let ``shouldn't break on one-line comment`` () =
    formatSourceString
        false
        """
1 + (* Comment *) 1"""
        config
    |> prepend newline
    |> should
        equal
        """
1 + (* Comment *) 1
"""

[<Test>]
let ``should keep comments on DU cases`` () =
    formatSourceString
        false
        """
/// XML comment
type X =
   /// Hello
   A
   /// Goodbye
   | B
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// XML comment
type X =
    /// Hello
    | A
    /// Goodbye
    | B
"""

[<Test>]
let ``should keep comments before attributes`` () =
    formatSourceString
        false
        """
[<NoEquality; NoComparison>]
type IlxGenOptions =
    { fragName: string
      generateFilterBlocks: bool
      workAroundReflectionEmitBugs: bool
      emitConstantArraysUsingStaticDataBlobs: bool
      // If this is set, then the last module becomes the "main" module and its toplevel bindings are executed at startup
      mainMethodInfo: Tast.Attribs option
      localOptimizationsAreOn: bool
      generateDebugSymbols: bool
      testFlagEmitFeeFeeAs100001: bool
      ilxBackend: IlxGenBackend
      /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
      /// This includes all interactively compiled code, including #load, definitions, and expressions
      isInteractive: bool
      // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
      // storage, even though 'it' is not logically mutable
      isInteractiveItExpr: bool
      // Indicates System.SerializableAttribute is available in the target framework
      netFxHasSerializableAttribute : bool
      /// Whenever possible, use callvirt instead of call
      alwaysCallVirt: bool}

"""
        config
    |> prepend newline
    |> should
        equal
        """
[<NoEquality; NoComparison>]
type IlxGenOptions =
    {
        fragName: string
        generateFilterBlocks: bool
        workAroundReflectionEmitBugs: bool
        emitConstantArraysUsingStaticDataBlobs: bool
        // If this is set, then the last module becomes the "main" module and its toplevel bindings are executed at startup
        mainMethodInfo: Tast.Attribs option
        localOptimizationsAreOn: bool
        generateDebugSymbols: bool
        testFlagEmitFeeFeeAs100001: bool
        ilxBackend: IlxGenBackend
        /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
        /// This includes all interactively compiled code, including #load, definitions, and expressions
        isInteractive: bool
        // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
        // storage, even though 'it' is not logically mutable
        isInteractiveItExpr: bool
        // Indicates System.SerializableAttribute is available in the target framework
        netFxHasSerializableAttribute: bool
        /// Whenever possible, use callvirt instead of call
        alwaysCallVirt: bool
    }
"""

[<Test>]
let ``should keep comments on else if`` () =
    formatSourceString
        false
        """
if a then ()
else
    // Comment 1
    if b then ()
    // Comment 2
    else ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    ()
else
// Comment 1
if
    b
then
    ()
// Comment 2
else
    ()
"""

[<Test>]
let ``should keep comments on almost-equal identifiers`` () =
    formatSourceString
        false
        """
let zp = p1 ``lxor`` p2
// Comment 1
let b = zp ``land`` (zp)
(* Comment 2 *)
let p = p1 ``land`` (b - 1)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let zp = p1 ``lxor`` p2
// Comment 1
let b = zp ``land`` (zp)
(* Comment 2 *)
let p = p1 ``land`` (b - 1)
"""

[<Test>]
let ``should not write sticky-to-the-left comments in a new line`` () =
    formatSourceString
        false
        """
let moveFrom source =
  getAllFiles source
    |> Seq.filter (fun f -> Path.GetExtension(f).ToLower() <> ".db")  //exlcude the thumbs.db files
    |> move @"C:\_EXTERNAL_DRIVE\_Camera"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let moveFrom source =
    getAllFiles source
    |> Seq.filter (fun f -> Path.GetExtension(f).ToLower() <> ".db") //exlcude the thumbs.db files
    |> move @"C:\_EXTERNAL_DRIVE\_Camera"
"""

[<Test>]
let ``should handle comments at the end of file`` () =
    formatSourceString
        false
        """
let hello() = "hello world"

(* This is a comment. *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let hello () = "hello world"

(* This is a comment. *)
"""

[<Test>]
let ``should handle block comments at the end of file, 810`` () =
    formatSourceString
        false
        """
printfn "hello world"
(* This is a comment. *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
printfn "hello world"
(* This is a comment. *)
"""

[<Test>]
let ``preserve block comment after record, 516`` () =
    formatSourceString
        false
        """module TriviaModule =

let env = "DEBUG"

type Config = {
    Name: string
    Level: int
}

let meh = { // this comment right
    Name = "FOO"; Level = 78 }

(* ending with block comment *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
module TriviaModule =

    let env = "DEBUG"

    type Config = { Name: string; Level: int }

    let meh =
        { // this comment right
          Name = "FOO"
          Level = 78 }

(* ending with block comment *)
"""

[<Test>]
let ``should keep comments inside unit`` () =
    formatSourceString
        false
        """
let x =
    ((*comment*))
    printf "a"
    // another comment 1
    printf "b"
    // another comment 2
    printf "c"

"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    ( (*comment*) )
    printf "a"
    // another comment 1
    printf "b"
    // another comment 2
    printf "c"
"""

[<Test>]
let ``preserve newline false should not add additional newline`` () =
    let source =
        """
type T() =
    let x = 123
//    override private x.ToString() = ""
"""

    formatSourceString false source config
    |> prepend newline
    |> should
        equal
        """
type T() =
    let x = 123
//    override private x.ToString() = ""
"""

[<Test>]
let ``comment after function in type definition should be applied to member bindings`` () =
    formatSourceString
        false
        """
type C () =
    let rec g x = h x
    and h x = g x

    member x.P = g 3"""
        config
    |> prepend newline
    |> should
        equal
        """
type C() =
    let rec g x = h x
    and h x = g x

    member x.P = g 3
"""

[<Test>]
let ``line comment with only two slashes`` () =
    let source =
        """
let foo = 7
//
"""

    formatSourceString false source config
    |> should
        equal
        """let foo = 7
//
"""

[<Test>]
let ``block comment on top of namespace`` () =
    formatSourceString
        false
        """
(*

Copyright 2010-2012 TidePowerd Ltd.
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace ExtCore"""
        config
    |> should
        equal
        """(*

Copyright 2010-2012 TidePowerd Ltd.
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace ExtCore
"""

[<Test>]
let ``block comment on top of file`` () =
    formatSourceString
        false
        """
(*

Copyright 2010-2012 TidePowerd Ltd.
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace ExtCore

open System
//open System.Diagnostics.Contracts
open System.Globalization
open System.Runtime.InteropServices


/// Represents a segment of a string.
[<Struct; CompiledName("Substring")>]
[<CustomEquality; CustomComparison>]
type substring =
    /// The underlying string for this substring.
    val String : string
    /// The position of the first character in the substring, relative to the start of the underlying string.
    val Offset : int
    /// The number of characters spanned by the substring.
    val Length : int

    /// <summary>Create a new substring value spanning the entirety of a specified string.</summary>
    /// <param name="string">The string to use as the substring's underlying string.</param>
    new (string : string) =
        // Preconditions
        checkNonNull "string" string

        { String = string;
          Offset = 0;
          Length = string.Length; }

    /// <summary>
    /// Compares two specified <see cref="substring"/> objects by evaluating the numeric values of the corresponding
    /// <see cref="Char"/> objects in each substring.
    /// </summary>
    /// <param name="strA">The first string to compare.</param>
    /// <param name="strB">The second string to compare.</param>
    /// <returns>An integer that indicates the lexical relationship between the two comparands.</returns>
    static member CompareOrdinal (strA : substring, strB : substring) =
        // If both substrings are empty they are considered equal, regardless of their offset or underlying string.
        if strA.Length = 0 && strB.Length = 0 then 0

        // OPTIMIZATION : If the substrings have the same (identical) underlying string
        // and offset, the comparison value will depend only on the length of the substrings.
        elif strA.String == strB.String && strA.Offset = strB.Offset then
            compare strA.Length strB.Length

        else
            (* Structural comparison on substrings -- this uses the same comparison
               technique as the structural comparison on strings in FSharp.Core. *)
#if INVARIANT_CULTURE_STRING_COMPARISON
            // NOTE: we don't have to null check here because System.String.Compare
            // gives reliable results on null values.
            System.String.Compare (
                strA.String, strA.Offset,
                strB.String, strB.Offset,
                min strA.Length strB.Length,
                false,
                CultureInfo.InvariantCulture)
#else
            // NOTE: we don't have to null check here because System.String.CompareOrdinal
            // gives reliable results on null values.
            System.String.CompareOrdinal (
                strA.String, strA.Offset,
                strB.String, strB.Offset,
                min strA.Length strB.Length)
#endif
"""
        { config with MaxInfixOperatorExpression = 60 }
    |> should
        equal
        """(*

Copyright 2010-2012 TidePowerd Ltd.
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace ExtCore

open System
//open System.Diagnostics.Contracts
open System.Globalization
open System.Runtime.InteropServices


/// Represents a segment of a string.
[<Struct; CompiledName("Substring")>]
[<CustomEquality; CustomComparison>]
type substring =
    /// The underlying string for this substring.
    val String: string
    /// The position of the first character in the substring, relative to the start of the underlying string.
    val Offset: int
    /// The number of characters spanned by the substring.
    val Length: int

    /// <summary>Create a new substring value spanning the entirety of a specified string.</summary>
    /// <param name="string">The string to use as the substring's underlying string.</param>
    new(string: string) =
        // Preconditions
        checkNonNull "string" string

        { String = string
          Offset = 0
          Length = string.Length }

    /// <summary>
    /// Compares two specified <see cref="substring"/> objects by evaluating the numeric values of the corresponding
    /// <see cref="Char"/> objects in each substring.
    /// </summary>
    /// <param name="strA">The first string to compare.</param>
    /// <param name="strB">The second string to compare.</param>
    /// <returns>An integer that indicates the lexical relationship between the two comparands.</returns>
    static member CompareOrdinal(strA: substring, strB: substring) =
        // If both substrings are empty they are considered equal, regardless of their offset or underlying string.
        if strA.Length = 0 && strB.Length = 0 then
            0

        // OPTIMIZATION : If the substrings have the same (identical) underlying string
        // and offset, the comparison value will depend only on the length of the substrings.
        elif strA.String == strB.String && strA.Offset = strB.Offset then
            compare strA.Length strB.Length

        else
            (* Structural comparison on substrings -- this uses the same comparison
               technique as the structural comparison on strings in FSharp.Core. *)
#if INVARIANT_CULTURE_STRING_COMPARISON
            // NOTE: we don't have to null check here because System.String.Compare
            // gives reliable results on null values.
            System.String.Compare(
                strA.String,
                strA.Offset,
                strB.String,
                strB.Offset,
                min strA.Length strB.Length,
                false,
                CultureInfo.InvariantCulture
            )
#else
            // NOTE: we don't have to null check here because System.String.CompareOrdinal
            // gives reliable results on null values.
            System.String.CompareOrdinal(
                strA.String,
                strA.Offset,
                strB.String,
                strB.Offset,
                min strA.Length strB.Length
            )
#endif
"""

[<Test>]
let ``single block comment in namespace, 1951`` () =
    formatSourceString
        false
        """
namespace ASTViewer.Server
(* open Microsoft.Azure.Functions.Worker.Http
open Microsoft.Azure.Functions.Worker
open Microsoft.Extensions.Logging *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace ASTViewer.Server
(* open Microsoft.Azure.Functions.Worker.Http
open Microsoft.Azure.Functions.Worker
open Microsoft.Extensions.Logging *)
"""

[<Test>]
let ``line comment after "then"`` () =
    formatSourceString
        false
        """
if true then //comment
    1
else 0"""
        config
    |> prepend newline
    |> should
        equal
        """
if true then //comment
    1
else
    0
"""

[<Test>]
let ``line comment after "if"`` () =
    formatSourceString
        false
        """
if //comment
    true then 1
else 0"""
        config
    |> prepend newline
    |> should
        equal
        """
if //comment
    true
then
    1
else
    0
"""

[<Test>]
let ``line comment after "else"`` () =
    formatSourceString
        false
        """
if true then 1
else //comment
    0"""
        config
    |> prepend newline
    |> should
        equal
        """
if true then
    1
else //comment
    0
"""

[<Test>]
let ``comments for enum cases, 572`` () =
    formatSourceString
        false
        """type A =
    /// Doc for CaseA
    | CaseA = 0
    /// Doc for CaseB
    | CaseB = 1
    /// Doc for CaseC
    | CaseC = 2"""
        config
    |> should
        equal
        """type A =
    /// Doc for CaseA
    | CaseA = 0
    /// Doc for CaseB
    | CaseB = 1
    /// Doc for CaseC
    | CaseC = 2
"""

    formatSourceString
        false
        """type A =
    // Comment for CaseA
    | CaseA = 0
    // Comment for CaseB
    | CaseB = 1
    // Comment for CaseC
    | CaseC = 2"""
        config
    |> should
        equal
        """type A =
    // Comment for CaseA
    | CaseA = 0
    // Comment for CaseB
    | CaseB = 1
    // Comment for CaseC
    | CaseC = 2
"""

[<Test>]
let ``comments in multi-pattern case matching should not be removed, 813`` () =
    formatSourceString
        false
        """
let f x =
    match x with
    | A // inline comment
    // line comment
    | B -> Some()
    | _ -> None"""
        config
    |> prepend newline
    |> should
        equal
        """
let f x =
    match x with
    | A // inline comment
    // line comment
    | B -> Some()
    | _ -> None
"""

[<Test>]
let ``block comments in multi-pattern case matching should not be removed`` () =
    formatSourceString
        false
        """
let f x =
    match x with
    | A
    (* multi-line
       block comment *)
    | B -> Some()
    | _ -> None"""
        config
    |> prepend newline
    |> should
        equal
        """
let f x =
    match x with
    | A
    (* multi-line
       block comment *)
    | B -> Some()
    | _ -> None
"""

[<Test>]
let ``multiple line comments form a single trivia`` () =
    formatSourceString
        false
        """
/// Represents a long identifier with possible '.' at end.
///
/// Typically dotms.Length = lid.Length-1, but they may be same if (incomplete) code ends in a dot, e.g. "Foo.Bar."
/// The dots mostly matter for parsing, and are typically ignored by the typechecker, but
/// if dotms.Length = lid.Length, then the parser must have reported an error, so the typechecker is allowed
/// more freedom about typechecking these expressions.
/// LongIdent can be empty list - it is used to denote that name of some AST element is absent (i.e. empty type name in inherit)
type LongIdentWithDots =
    | LongIdentWithDots of id: LongIdent * dotms: range list
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Represents a long identifier with possible '.' at end.
///
/// Typically dotms.Length = lid.Length-1, but they may be same if (incomplete) code ends in a dot, e.g. "Foo.Bar."
/// The dots mostly matter for parsing, and are typically ignored by the typechecker, but
/// if dotms.Length = lid.Length, then the parser must have reported an error, so the typechecker is allowed
/// more freedom about typechecking these expressions.
/// LongIdent can be empty list - it is used to denote that name of some AST element is absent (i.e. empty type name in inherit)
type LongIdentWithDots = LongIdentWithDots of id: LongIdent * dotms: range list
"""

[<Test>]
let ``newline between comments should lead to individual comments, 920`` () =
    formatSourceString
        false
        """
[<AllowNullLiteral>]
type IExports =
    abstract DataSet: DataSetStatic
    abstract DataView: DataViewStatic
    abstract Graph2d: Graph2dStatic
    abstract Timeline: TimelineStatic
    // abstract Timeline: TimelineStaticStatic
    abstract Network: NetworkStatic

// type [<AllowNullLiteral>] MomentConstructor1 =
//     [<Emit "$0($1...)">] abstract Invoke: ?inp: MomentInput * ?format: MomentFormatSpecification * ?strict: bool -> Moment

// type [<AllowNullLiteral>] MomentConstructor2 =
//     [<Emit "$0($1...)">] abstract Invoke: ?inp: MomentInput * ?format: MomentFormatSpecification * ?language: string * ?strict: bool -> Moment

// type MomentConstructor =
//     U2<MomentConstructor1, MomentConstructor2>
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type IExports =
    abstract DataSet: DataSetStatic
    abstract DataView: DataViewStatic
    abstract Graph2d: Graph2dStatic
    abstract Timeline: TimelineStatic
    // abstract Timeline: TimelineStaticStatic
    abstract Network: NetworkStatic

// type [<AllowNullLiteral>] MomentConstructor1 =
//     [<Emit "$0($1...)">] abstract Invoke: ?inp: MomentInput * ?format: MomentFormatSpecification * ?strict: bool -> Moment

// type [<AllowNullLiteral>] MomentConstructor2 =
//     [<Emit "$0($1...)">] abstract Invoke: ?inp: MomentInput * ?format: MomentFormatSpecification * ?language: string * ?strict: bool -> Moment

// type MomentConstructor =
//     U2<MomentConstructor1, MomentConstructor2>
"""

[<Test>]
let ``comment newline comment`` () =
    formatSourceString
        false
        """
// foo

// bar
let x = 8
"""
        config
    |> prepend newline
    |> should
        equal
        """
// foo

// bar
let x = 8
"""

[<Test>]
let ``comments before no warn should not be removed, 1220`` () =
    formatSourceString
        false
        """
// sample comment before no warn
#nowarn "44"

namespace Foo

module Bar =
    let Baz () =
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
// sample comment before no warn
#nowarn "44"

namespace Foo

module Bar =
    let Baz () = ()
"""

[<Test>]
let ``comment above do keyword, 1343`` () =
    formatSourceString
        false
        """
if stateSub.Value |> State.hasChanges then
    // Push changes
    let! pushed = push state

    // Import new etags of pushed items
    do
        pushed
        |> Option.bind Dto.changesAsImport
        |> Option.iter (fun changes -> update (Import changes) |> ignore)
"""
        config
    |> prepend newline
    |> should
        equal
        """
if stateSub.Value |> State.hasChanges then
    // Push changes
    let! pushed = push state

    // Import new etags of pushed items
    do
        pushed
        |> Option.bind Dto.changesAsImport
        |> Option.iter (fun changes -> update (Import changes) |> ignore)
"""

[<Test>]
let ``very long comment on single line`` () =
    formatSourceString
        false
        """
// Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ipsum nulla, pellentesque eget maximus et, facilisis eu nibh. Suspendisse convallis scelerisque urna, id fringilla dolor mollis id. Etiam dictum pellentesque nisl, vel ullamcorper neque accumsan eget. Pellentesque at mattis magna. Cras varius nisl nisi, sed iaculis tortor auctor quis. Sed luctus eget ante in dapibus. Cras ac leo nibh. Sed commodo, ex vel interdum egestas, risus lorem volutpat sapien, at pellentesque lectus ipsum non libero. Pellentesque malesuada scelerisque augue at blandit. Fusce in nisl sapien. In hac habitasse platea dictumst. Aenean tristique nibh ac tortor laoreet, rutrum aliquam elit rutrum. In vitae dignissim neque.
// Dollar
let meh =   7
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ipsum nulla, pellentesque eget maximus et, facilisis eu nibh. Suspendisse convallis scelerisque urna, id fringilla dolor mollis id. Etiam dictum pellentesque nisl, vel ullamcorper neque accumsan eget. Pellentesque at mattis magna. Cras varius nisl nisi, sed iaculis tortor auctor quis. Sed luctus eget ante in dapibus. Cras ac leo nibh. Sed commodo, ex vel interdum egestas, risus lorem volutpat sapien, at pellentesque lectus ipsum non libero. Pellentesque malesuada scelerisque augue at blandit. Fusce in nisl sapien. In hac habitasse platea dictumst. Aenean tristique nibh ac tortor laoreet, rutrum aliquam elit rutrum. In vitae dignissim neque.
// Dollar
let meh = 7
"""

[<Test>]
let ``comment after semi colon in record definition, 1643`` () =
    formatSourceString
        false
        """
type T =
  { id : int
  ; value : RT.Dval
  ; retries : int
  ; canvasID : CanvasID
  ; canvasName : string
  ; module_ : string
  ; name : string
  ; modifier : string
  ; // Delay in ms since it entered the queue
    delay : float }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T =
    { id: int
      value: RT.Dval
      retries: int
      canvasID: CanvasID
      canvasName: string
      module_: string
      name: string
      modifier: string
      // Delay in ms since it entered the queue
      delay: float }
"""

[<Test>]
let ``comment after semicolon`` () =
    formatSourceString
        false
        """
let a = 8 ; // foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 8 // foobar
"""

[<Test>]
let ``comment after semicolon on next line`` () =
    formatSourceString
        false
        """
let a = 8
; // foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 8
// foobar
"""

[<Test>]
let ``file end with newline followed by comment, 1649`` () =
    formatSourceString
        false
        """
#load "Hi.fsx"
open Something

//// FOO
[
    1
    2
    3
]

//// The end
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBlockBracketsOnSameColumn = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true
            ExperimentalKeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
#load "Hi.fsx"
open Something

//// FOO
[ 1 ; 2 ; 3 ]

//// The end
"""

[<Test>]
let ``block comment above let binding`` () =
    formatSourceString
        false
        """(* meh *)
let a =  b
"""
        config
    |> prepend newline
    |> should
        equal
        """
(* meh *)
let a = b
"""

[<Test>]
let ``comment right after first token`` () =
    formatSourceString
        false
        """
1//
// next line
"""
        config
    |> prepend newline
    |> should
        equal
        """
1 //
// next line
"""

[<Test>]
[<Ignore("line comment after block comment currently not supported")>]
let ``block comment followed by line comment`` () =
    formatSourceString
        false
        """
(* foo *)// bar
let a = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
(* foo *) // bar
let a = 0
"""

[<Test>]
let ``line comment after source code`` () =
    formatSourceString
        false
        """
__SOURCE_DIRECTORY__ // comment
"""
        config
    |> prepend newline
    |> should
        equal
        """
__SOURCE_DIRECTORY__ // comment
"""

[<Test>]
let ``line comment after hash define`` () =
    formatSourceString
        false
        """
#if FOO // MEH
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO // MEH
#endif
"""

[<Test>]
let ``line comment after interpolated string`` () =
    formatSourceString
        false
        """
$"{meh}.." // foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"{meh}.." // foo
"""

[<Test>]
let ``line comment after negative constant`` () =
    formatSourceString
        false
        """
-1.0 // foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
-1.0 // foo
"""

[<Test>]
let ``line comment after trivia number`` () =
    formatSourceString
        false
        """
1. // bar
"""
        config
    |> prepend newline
    |> should
        equal
        """
1. // bar
"""

[<Test>]
let ``line comment after infix operator in full words`` () =
    formatSourceString
        false
        """
op_LessThan // meh
"""
        config
    |> prepend newline
    |> should
        equal
        """
op_LessThan // meh
"""

[<Test>]
let ``line comment after ident between ticks`` () =
    formatSourceString
        false
        """
``foo oo`` // bar
"""
        config
    |> prepend newline
    |> should
        equal
        """
``foo oo`` // bar
"""

[<Test>]
let ``line comment after special char`` () =
    formatSourceString
        false
        """
'\u0000' // foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
'\u0000' // foo
"""

[<Test>]
let ``line comment after embedded il`` () =
    formatSourceString
        false
        """
(# "" x : 'U #) // bar
"""
        config
    |> prepend newline
    |> should
        equal
        """
(# "" x : 'U #) // bar
"""

[<Test>]
let ``line comment before SynExpr.AddressOf`` () =
    formatSourceString
        false
        """
open FSharp.NativeInterop

let Main() =
  let mutable x = 3.1415
  // meh?
  &&x
"""
        config
    |> prepend newline
    |> should
        equal
        """
open FSharp.NativeInterop

let Main () =
    let mutable x = 3.1415
    // meh?
    &&x
"""

[<Test>]
let ``line comment after SynExpr.Null, 1676`` () =
    formatSourceString
        false
        """
let v = f null // comment
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v = f null // comment
"""

[<Test>]
let ``newline followed by line comment at end of file, 1468`` () =
    formatSourceString
        false
        """
Host
    .CreateDefaultBuilder()
    .ConfigureWebHostDefaults(fun webHostBuilder ->
        webHostBuilder
            .Configure(configureApp)
            .ConfigureServices(configureServices)
        |> ignore)
    .Build()
    .Run()

//
"""
        { config with MaxDotGetExpressionWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
Host
    .CreateDefaultBuilder()
    .ConfigureWebHostDefaults(fun webHostBuilder ->
        webHostBuilder
            .Configure(configureApp)
            .ConfigureServices(configureServices)
        |> ignore)
    .Build()
    .Run()

//
"""

[<Test>]
let ``comment after bracket in record should not be duplicated in computation expression, 1912`` () =
    formatSourceString
        false
        """
type TorDirectory =
    private
        {
            NetworkStatus: NetworkStatusDocument
        }

    static member Bootstrap (nodeEndPoint: IPEndPoint) =
        async {
            return
                {
                    TorDirectory.NetworkStatus =
                        NetworkStatusDocument.Parse consensusStr
                    ServerDescriptors = Map.empty
                    // comment
                }
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type TorDirectory =
    private
        { NetworkStatus: NetworkStatusDocument }

    static member Bootstrap(nodeEndPoint: IPEndPoint) =
        async {
            return
                { TorDirectory.NetworkStatus = NetworkStatusDocument.Parse consensusStr
                  ServerDescriptors = Map.empty
                // comment
                }
        }
"""

[<Test>]
let ``double try-with, comment before inner 'with' not duplicated, 1969`` () =
    formatSourceString
        false
        """
try
    try
        ()
        // xxx
    with
    | _ -> ()
with
| _ -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    try
        ()
    // xxx
    with _ ->
        ()
with _ ->
    ()
"""

[<Test>]
let ``comment should not be lost`` () =
    formatSourceString
        false
        """
try
    a
// comment
with


| b -> c
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    a
// comment
with


| b ->
    c
"""

[<Test>]
let ``nested try/with with comment on with`` () =
    formatSourceString
        false
        """
try
    a
with
| b ->
    try c
    // inner comment
    with
    | d -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    a
with b ->
    try
        c
    // inner comment
    with d ->
        ()
"""

[<Test>]
let ``trailing spaces in comments should be removed`` () =
    formatSourceString
        false
        """
// foo       
// bar          
let a = 9
"""
        config
    |> prepend newline
    |> should
        equal
        """
// foo
// bar
let a = 9
"""

[<Test>]
let ``comment after SynTypar, 2052`` () =
    formatSourceString
        false
        """
let Foo<'T (* TODO *)> () = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Foo<'T (* TODO *) > () = ()
"""

[<Test>]
let ``comment after SynTypeApp idents, 1899`` () =
    formatSourceString
        false
        """
[<RequireQualifiedAccess>]
module Example =

  let dict1 = ConcurrentDictionary< (* some comment 1 *) int32, unit>()
  let dict2 = ConcurrentDictionary< (* some comment 2 *) int64, ConcurrentDictionary<(* some comment 3 *) int32, unit>>()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<RequireQualifiedAccess>]
module Example =

    let dict1 = ConcurrentDictionary< (* some comment 1 *) int32, unit>()

    let dict2 =
        ConcurrentDictionary< (* some comment 2 *) int64, ConcurrentDictionary< (* some comment 3 *) int32, unit>>()
"""

[<Test>]
let ``correctly collect a double slash comment before a xml doc comment, 2152`` () =
    formatSourceString
        false
        """
// Maybe computation expression builder, copied from ExtCore library
/// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
[<Sealed>]
type MaybeBuilder() = class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Maybe computation expression builder, copied from ExtCore library
/// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
[<Sealed>]
type MaybeBuilder() =
    class
    end
"""

[<Test>]
let ``restore triple slash comment at invalid location`` () =
    formatSourceString
        false
        """
/// Valid xml doc
let x =
    /// Some great documentation comment
    /// With a blank line in between
    /// but on a while loop
    while true do ()
    a + 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Valid xml doc
let x =
    /// Some great documentation comment
    /// With a blank line in between
    /// but on a while loop
    while true do
        ()

    a + 1
"""

[<Test>]
let ``xml comment above double slash comment, 2186`` () =
    formatSourceString
        false
        """
/// <summary>
/// Some comment
/// </summary>
// type TestUnion = First | Second of int | Third of string
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// <summary>
/// Some comment
/// </summary>
// type TestUnion = First | Second of int | Third of string
"""

[<Test>]
let ``block comment should be attached to else expr`` () =
    formatSourceString
        false
        """
let compilerOptionUsage (CompilerOption (s, tag, spec, _, _)) =
    let s =
        if s = "--" then
            ""
        else
            s (* s="flag" for "--flag" options. s="--" for "--" option. Adjust printing here for "--" case. *)
    
    s
"""
        { config with MaxIfThenElseShortWidth = 60 }
    |> prepend newline
    |> should
        equal
        """
let compilerOptionUsage (CompilerOption (s, tag, spec, _, _)) =
    let s =
        if s = "--" then
            ""
        else
            s (* s="flag" for "--flag" options. s="--" for "--" option. Adjust printing here for "--" case. *)

    s
"""

[<Test>]
let ``block comment should be attached to last argument of application`` () =
    formatSourceString
        false
        """
match meh with
| OptionGeneral _ ->
    if tag = "" then
        sprintf "%s" s
    else
        sprintf "%s:%s" s tag (* still being decided *)
"""
        { config with MaxIfThenElseShortWidth = 60 }
    |> prepend newline
    |> should
        equal
        """
match meh with
| OptionGeneral _ ->
    if tag = "" then
        sprintf "%s" s
    else
        sprintf "%s:%s" s tag (* still being decided *)
"""

[<Test>]
let ``block comment after let decl`` () =
    formatSourceString
        false
        """
let compilerOptionUsage (CompilerOption (s, tag, spec, _, _)) =
    let s =
        if s = "--" then
            ""
        else
            s (* s="flag" for "--flag" options. s="--" for "--" option. Adjust printing here for "--" case. *)

    match spec with
    | OptionUnit _
    | OptionSet _
    | OptionClear _
    | OptionHelp _ -> sprintf "--%s" s
    | OptionStringList _ -> sprintf "--%s:%s" s tag
    | OptionIntList _ -> sprintf "--%s:%s" s tag
    | OptionSwitch _ -> sprintf "--%s[+|-]" s
    | OptionStringListSwitch _ -> sprintf "--%s[+|-]:%s" s tag
    | OptionIntListSwitch _ -> sprintf "--%s[+|-]:%s" s tag
    | OptionString _ -> sprintf "--%s:%s" s tag
    | OptionInt _ -> sprintf "--%s:%s" s tag
    | OptionFloat _ -> sprintf "--%s:%s" s tag
    | OptionRest _ -> sprintf "--%s ..." s
    | OptionGeneral _ ->
        if tag = "" then
            sprintf "%s" s
        else
            sprintf "%s:%s" s tag (* still being decided *)
"""
        { config with MaxIfThenElseShortWidth = 60 }
    |> prepend newline
    |> should
        equal
        """
let compilerOptionUsage (CompilerOption (s, tag, spec, _, _)) =
    let s =
        if s = "--" then
            ""
        else
            s (* s="flag" for "--flag" options. s="--" for "--" option. Adjust printing here for "--" case. *)

    match spec with
    | OptionUnit _
    | OptionSet _
    | OptionClear _
    | OptionHelp _ -> sprintf "--%s" s
    | OptionStringList _ -> sprintf "--%s:%s" s tag
    | OptionIntList _ -> sprintf "--%s:%s" s tag
    | OptionSwitch _ -> sprintf "--%s[+|-]" s
    | OptionStringListSwitch _ -> sprintf "--%s[+|-]:%s" s tag
    | OptionIntListSwitch _ -> sprintf "--%s[+|-]:%s" s tag
    | OptionString _ -> sprintf "--%s:%s" s tag
    | OptionInt _ -> sprintf "--%s:%s" s tag
    | OptionFloat _ -> sprintf "--%s:%s" s tag
    | OptionRest _ -> sprintf "--%s ..." s
    | OptionGeneral _ ->
        if tag = "" then
            sprintf "%s" s
        else
            sprintf "%s:%s" s tag (* still being decided *)
"""

[<Test>]
let ``comment after long parameter signature`` () =
    formatSourceString
        false
        """
type CreateFSharpManifestResourceName public () =
    inherit CreateCSharpManifestResourceName()

    member val UseStandardResourceNames = false with get, set

    override this.CreateManifestName
        (
            fileName: string,
            linkFileName: string,
            rootNamespace: string,  // may be null
            dependentUponFileName: string,  // may be null
            binaryStream: Stream // may be null
        ) : string =
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type CreateFSharpManifestResourceName public () =
    inherit CreateCSharpManifestResourceName()

    member val UseStandardResourceNames = false with get, set

    override this.CreateManifestName
        (
            fileName: string,
            linkFileName: string,
            rootNamespace: string,  // may be null
            dependentUponFileName: string,  // may be null
            binaryStream: Stream // may be null
        ) : string =
        ()
"""

[<Test>]
let ``comment after type in tuple`` () =
    formatSourceString
        false
        """
[<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
type ILNativeType =
    | UInt64
    | Array of
        ILNativeType option *
        (int32 * int32 option) option (* optional idx of parameter giving size plus optional additive i.e. num elems *)
    | Int
    | UInt
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
type ILNativeType =
    | UInt64
    | Array of
        ILNativeType option *
        (int32 * int32 option) option (* optional idx of parameter giving size plus optional additive i.e. num elems *)
    | Int
    | UInt
"""

[<Test>]
let ``comment after entire member`` () =
    formatSourceString
        false
        """
type Meh () =
    member x.IsComInterop =
        x.Attributes &&& TypeAttributes.Import
        <> enum 0 (* Class or interface generated for COM interop *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh() =
    member x.IsComInterop =
        x.Attributes &&& TypeAttributes.Import
        <> enum 0 (* Class or interface generated for COM interop *)
"""

[<Test>]
let ``comment after val in signature file`` () =
    formatSourceString
        true
        """
/// Not all custom attribute data can be decoded without binding types.  In particular
/// enums must be bound in order to discover the size of the underlying integer.
/// The following assumes enums have size int32.
val internal decodeILAttribData:
    ILAttribute ->
        ILAttribElem list (* fixed args *) *
        ILAttributeNamedArg list (* named args: values and flags indicating if they are fields or properties *)
        
val internal mkILCustomAttribMethRef:
    ILMethodSpec *
    ILAttribElem list (* fixed args: values and implicit types *) *
    ILAttributeNamedArg list (* named args: values and flags indicating if they are fields or properties *) ->
        ILAttribute

val pdbReaderGetMethodFromDocumentPosition: PdbReader -> PdbDocument -> int (* line *)  -> int (* col *)  -> PdbMethod
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Not all custom attribute data can be decoded without binding types.  In particular
/// enums must be bound in order to discover the size of the underlying integer.
/// The following assumes enums have size int32.
val internal decodeILAttribData:
    ILAttribute ->
        ILAttribElem list (* fixed args *) *
        ILAttributeNamedArg list (* named args: values and flags indicating if they are fields or properties *)

val internal mkILCustomAttribMethRef:
    ILMethodSpec *
    ILAttribElem list (* fixed args: values and implicit types *) *
    ILAttributeNamedArg list (* named args: values and flags indicating if they are fields or properties *) ->
        ILAttribute

val pdbReaderGetMethodFromDocumentPosition: PdbReader -> PdbDocument -> int (* line *) -> int (* col *) -> PdbMethod
"""

[<Test>]
let ``comment after nested module`` () =
    formatSourceString
        false
        """
module TableNames =
    let MethodSpec = TableName 43
    let GenericParamConstraint = TableName 44

    let UserStrings =
        TableName 0x70 (* Special encoding of embedded UserString tokens - See 1.9 Partition III *)

module NextModule =
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
module TableNames =
    let MethodSpec = TableName 43
    let GenericParamConstraint = TableName 44

    let UserStrings =
        TableName 0x70 (* Special encoding of embedded UserString tokens - See 1.9 Partition III *)

module NextModule =
    ()
"""

[<Test>]
let ``comment after lambda`` () =
    formatSourceString
        false
        """
let ilvs =
    lvs
    |> Array.toList
    |> List.filter (fun l ->
        let k, _idx = pdbVariableGetAddressAttributes l
        k = 1 (* ADDR_IL_OFFSET *) )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let ilvs =
    lvs
    |> Array.toList
    |> List.filter (fun l ->
        let k, _idx = pdbVariableGetAddressAttributes l
        k = 1 (* ADDR_IL_OFFSET *) )
"""

[<Test>]
let ``comment after union type`` () =
    formatSourceString
        true
        """
type IlxUnionHasHelpers =
    | NoHelpers
    | AllHelpers
    | SpecialFSharpListHelpers
    | SpecialFSharpOptionHelpers

/// Union references
type IlxUnionRef =
    | IlxUnionRef of
        boxity: ILBoxity *
        ILTypeRef *
        IlxUnionCase[] *
        bool (* IsNullPermitted *)  *
        IlxUnionHasHelpers (* HasHelpers *)

type IlxUnionSpec =
    | IlxUnionSpec of IlxUnionRef * ILGenericArgs

    member DeclaringType: ILType

    member GenericArgs: ILGenericArgs

    member Alternatives: IlxUnionCase list
"""
        config
    |> prepend newline
    |> should
        equal
        """
type IlxUnionHasHelpers =
    | NoHelpers
    | AllHelpers
    | SpecialFSharpListHelpers
    | SpecialFSharpOptionHelpers

/// Union references
type IlxUnionRef =
    | IlxUnionRef of
        boxity: ILBoxity *
        ILTypeRef *
        IlxUnionCase[] *
        bool (* IsNullPermitted *) *
        IlxUnionHasHelpers (* HasHelpers *)

type IlxUnionSpec =
    | IlxUnionSpec of IlxUnionRef * ILGenericArgs

    member DeclaringType: ILType

    member GenericArgs: ILGenericArgs

    member Alternatives: IlxUnionCase list
"""

[<Test>]
let ``comment above type in function signature`` () =
    formatSourceString
        true
        """
/// Compile a pattern into a decision tree and a set of targets.
val internal CompilePattern:
    TcGlobals ->
    DisplayEnv ->
    Import.ImportMap ->
    (ValRef -> ValUseFlag -> TTypes -> range -> Expr * TType) ->
    InfoReader ->
    // range of the expression we are matching on
    range ->
        a
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Compile a pattern into a decision tree and a set of targets.
val internal CompilePattern:
    TcGlobals ->
    DisplayEnv ->
    Import.ImportMap ->
    (ValRef -> ValUseFlag -> TTypes -> range -> Expr * TType) ->
    InfoReader ->
    // range of the expression we are matching on
    range ->
        a
"""

[<Test>]
let ``comment after LetOrUseBang`` () =
    formatSourceString
        false
        """
node {
    let! cachedResults =
        node {
            let! builderOpt, creationDiags = getAnyBuilder (options, userOpName)

            match builderOpt with
            | Some builder ->
                match! bc.GetCachedCheckFileResult(builder, fileName, sourceText, options) with
                | Some (_, checkResults) ->
                    return Some(builder, creationDiags, Some(FSharpCheckFileAnswer.Succeeded checkResults))
                | _ -> return Some(builder, creationDiags, None)
            | _ -> return None // the builder wasn't ready
        }

    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
node {
    let! cachedResults =
        node {
            let! builderOpt, creationDiags = getAnyBuilder (options, userOpName)

            match builderOpt with
            | Some builder ->
                match! bc.GetCachedCheckFileResult(builder, fileName, sourceText, options) with
                | Some (_, checkResults) ->
                    return Some(builder, creationDiags, Some(FSharpCheckFileAnswer.Succeeded checkResults))
                | _ -> return Some(builder, creationDiags, None)
            | _ -> return None // the builder wasn't ready
        }

    ()
}
"""

[<Test>]
let ``comment after parameters and before equals sign`` () =
    formatSourceString
        false
        """
let GetValueInfo bindingFlags (x: 'a, ty: Type) (* x could be null *)  =
    let obj = (box x)
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let GetValueInfo bindingFlags (x: 'a, ty: Type) (* x could be null *)  =
    let obj = (box x)
    ()
"""

[<Test>]
let ``comment between fun types`` () =
    formatSourceString
        true
        """
/// A set of function parameters (visitor) for folding over expressions
type ExprFolder<'State> =
    { exprIntercept: ('State -> Expr -> 'State) (* noInterceptF *)
        -> ('State -> Expr -> 'State)
        -> 'State
        -> Expr
        -> 'State (* recurseF *)
      valBindingSiteIntercept: 'State -> bool * Val -> 'State
      nonRecBindingsIntercept: 'State -> Binding -> 'State
      recBindingsIntercept: 'State -> Bindings -> 'State
      dtreeIntercept: 'State -> DecisionTree -> 'State
      targetIntercept: ('State -> Expr -> 'State) -> 'State -> DecisionTreeTarget -> 'State option
      tmethodIntercept: ('State -> Expr -> 'State) -> 'State -> ObjExprMethod -> 'State option }
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// A set of function parameters (visitor) for folding over expressions
type ExprFolder<'State> =
    { exprIntercept: ('State -> Expr -> 'State) (* noInterceptF *)
        -> ('State -> Expr -> 'State)
        -> 'State
        -> Expr
        -> 'State (* recurseF *)
      valBindingSiteIntercept: 'State -> bool * Val -> 'State
      nonRecBindingsIntercept: 'State -> Binding -> 'State
      recBindingsIntercept: 'State -> Bindings -> 'State
      dtreeIntercept: 'State -> DecisionTree -> 'State
      targetIntercept: ('State -> Expr -> 'State) -> 'State -> DecisionTreeTarget -> 'State option
      tmethodIntercept: ('State -> Expr -> 'State) -> 'State -> ObjExprMethod -> 'State option }
"""

[<Test>]
let ``block comments in type definition, 1975`` () =
    formatSourceString
        true
        """
module M

module A =
    type ProviderGeneratedType = ProviderGeneratedType of (*ilOrigTyRef*)ILTypeRef * (*ilRenamedTyRef*)ILTypeRef * ProviderGeneratedType list
"""
        config
    |> prepend newline
    |> should
        equal
        """
module M

module A =
    type ProviderGeneratedType =
        | ProviderGeneratedType (*ilOrigTyRef*) of ILTypeRef (*ilRenamedTyRef*) * ILTypeRef * ProviderGeneratedType list
"""

[<Test>]
let ``keep trailing spaces inside block comments, 2450`` () =
    formatSourceString
        false
        """
(**
## Auxiliary settings
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon type="orange"></fantomas-setting-icon>indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.  
Default = 4.
*)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(**
## Auxiliary settings
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon type="orange"></fantomas-setting-icon>indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.  
Default = 4.
*)
"""

[<Test>]
let ``trivia above tuple parameter in function type, 2149`` () =
    formatSourceString
        true
        """
namespace Foo

type X =
    static member AsBeginEnd : computation:('Arg -> Async<'T>) ->
                                    // The 'Begin' member
                                    ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) *
                                    // The 'End' member
                                    (System.IAsyncResult -> 'T) *
                                    // The 'Cancel' member
                                    (System.IAsyncResult -> unit)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type X =
    static member AsBeginEnd:
        computation: ('Arg -> Async<'T>) ->
            // The 'Begin' member
            ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) *
            // The 'End' member
            (System.IAsyncResult -> 'T) *
            // The 'Cancel' member
            (System.IAsyncResult -> unit)
"""

[<Test>]
let ``should keep mid quotation line comments, related to 2525`` () =
    formatSourceString
        false
        """test
    <@
      result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Replace("8.12", "8.13") // CRAP score rounding
        .Replace("4.12", "4.13") // CRAP score rounding
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |])
    @>
"""
        config
    |> should
        equal
        """test
    <@
        result
            .Replace('\r', '\u00FF')
            .Replace('\n', '\u00FF')
            .Replace("\u00FF\u00FF", "\u00FF")
            .Replace("8.12", "8.13") // CRAP score rounding
            .Replace("4.12", "4.13") // CRAP score rounding
            .Trim([| '\u00FF' |]) = expected
            .Replace('\r', '\u00FF')
            .Replace('\n', '\u00FF')
            .Replace("\u00FF\u00FF", "\u00FF")
            .Trim([| '\u00FF' |])
    @>
"""

[<Test>]
let ``should keep mid attribute line comments, 2525`` () =
    formatSourceString
        false
        """[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member",  // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "Bytecode delta only")>]
()
"""
        config
    |> should
        equal
        """[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "Bytecode delta only")>]
()
"""
