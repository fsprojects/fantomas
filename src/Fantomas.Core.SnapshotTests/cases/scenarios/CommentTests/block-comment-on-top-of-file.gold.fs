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
