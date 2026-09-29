namespace System.Diagnostics.CodeAnalysis

open System

/// Silences a trim or Native AOT analysis warning for code that has been read and found to be fine,
/// with the reason in `Justification`.
///
/// .NET 5 and later define this attribute, netstandard2.0 does not. The trimmer and the Native AOT
/// compiler recognise it by its full name and not by the assembly it comes from, so this copy is
/// honoured when the tool is published with Native AOT.
[<AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = true)>]
[<Sealed>]
type internal UnconditionalSuppressMessageAttribute(category: string, checkId: string) =
    inherit Attribute()

    member _.Category: string = category
    member _.CheckId: string = checkId
    member val Scope: string = null with get, set
    member val Target: string = null with get, set
    member val MessageId: string = null with get, set
    member val Justification: string = null with get, set
