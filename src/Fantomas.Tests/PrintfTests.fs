module Fantomas.Tests.PrintfTests

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open NUnit.Framework

// F# printf specializes its formatters at runtime through `MethodInfo.MakeGenericMethod`, which a
// Native AOT build of the tool cannot run. Which formats survive depends on what the AOT compiler
// happened to generate, so a call can work in one build and throw in the next: `profile` crashed on
// a `%.0f`, parse errors on a `%d`. The only safe rule is that the tool does not reach printf.
//
// Every printf use builds a `PrintfFormat`, whether it is written as `sprintf`, `failwithf` or an
// interpolated string the compiler did not lower to `String.Concat` or `String.Format`, which is
// every one with `%A`, `%O` or a width, a flag or a precision. So this reads the IL of the tool and
// of Fantomas.Core for those constructions, rather than the source for what might be one.
//
// `FANTOMAS-PRINTF-001` says the same while you type, from the source. This is the check against
// what the compiler actually emitted, and the places allowed below are the ones suppressed there.

/// Where a printf format is built: the function it is written in, and the line when the compiler
/// kept one.
type private PrintfUse =
    {
        Function: string
        MethodName: string
        Location: string option
    }

/// Each opcode by its value, for the size of the operand that follows it.
let private opCodes: IReadOnlyDictionary<int, OpCode> =
    let byValue: Dictionary<int, OpCode> = Dictionary<int, OpCode>()

    for field in typeof<OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static) do
        let opCode: OpCode = field.GetValue null :?> OpCode
        byValue.[int (uint16 opCode.Value)] <- opCode

    byValue

let private operandSize (opCode: OpCode) (il: byte array) (at: int) : int =
    match opCode.OperandType with
    | OperandType.InlineNone -> 0
    | OperandType.ShortInlineBrTarget
    | OperandType.ShortInlineI
    | OperandType.ShortInlineVar -> 1
    | OperandType.InlineVar -> 2
    | OperandType.InlineI8
    | OperandType.InlineR -> 8
    | OperandType.InlineSwitch -> 4 + 4 * BitConverter.ToInt32(il, at)
    | _ -> 4

/// The name of the type a member reference belongs to, looking through a generic instantiation,
/// which is how every `PrintfFormat` arrives.
let private parentName (metadata: MetadataReader) (parent: EntityHandle) : string =
    let referenceName (handle: EntityHandle) : string =
        if handle.Kind <> HandleKind.TypeReference then
            ""
        else
            metadata.GetString(metadata.GetTypeReference(TypeReferenceHandle.op_Explicit handle).Name)

    if parent.Kind <> HandleKind.TypeSpecification then
        referenceName parent
    else

    let specification: TypeSpecification =
        metadata.GetTypeSpecification(TypeSpecificationHandle.op_Explicit parent)

    let mutable signature: BlobReader = metadata.GetBlobReader specification.Signature

    if signature.ReadSignatureTypeCode() <> SignatureTypeCode.GenericTypeInstance then
        ""
    else

    signature.ReadSignatureTypeCode() |> ignore
    referenceName (signature.ReadTypeHandle())

let private buildsPrintfFormat (metadata: MetadataReader) (token: int) : bool =
    let handle: EntityHandle = MetadataTokens.EntityHandle token

    if handle.Kind <> HandleKind.MemberReference then
        false
    else

    let reference: MemberReference =
        metadata.GetMemberReference(MemberReferenceHandle.op_Explicit handle)

    metadata.GetString reference.Name = ".ctor"
    && (parentName metadata reference.Parent).StartsWith("PrintfFormat", StringComparison.Ordinal)

/// The function a method was written as. A lambda is compiled to a closure class named after the
/// function it sits in, `genNode@205`, so that is the name it goes by here.
let private functionOf (metadata: MetadataReader) (method: MethodDefinition) : string =
    let declaring: TypeDefinition =
        metadata.GetTypeDefinition(method.GetDeclaringType())

    let typeName: string = metadata.GetString declaring.Name

    match typeName.IndexOf '@' with
    | -1 -> $"%s{typeName}.%s{metadata.GetString method.Name}"
    | at ->

    let owner: TypeDefinitionHandle = declaring.GetDeclaringType()

    let ownerName: string =
        if owner.IsNil then
            ""
        else
            metadata.GetString(metadata.GetTypeDefinition(owner).Name)

    $"%s{ownerName}.%s{typeName.Substring(0, at)}"

let private printfUses (assembly: Assembly) : PrintfUse list =
    use pe: PEReader = new PEReader(File.OpenRead assembly.Location)
    let metadata: MetadataReader = pe.GetMetadataReader()

    let pdb: MetadataReader option =
        pe.ReadDebugDirectory()
        |> Seq.tryFind (fun (entry: DebugDirectoryEntry) -> entry.Type = DebugDirectoryEntryType.EmbeddedPortablePdb)
        |> Option.map (fun (entry: DebugDirectoryEntry) ->
            pe.ReadEmbeddedPortablePdbDebugDirectoryData(entry).GetMetadataReader()
        )

    let locationOf (handle: MethodDefinitionHandle) (offset: int) : string option =
        pdb
        |> Option.bind (fun (pdb: MetadataReader) ->
            pdb.GetMethodDebugInformation(handle.ToDebugInformationHandle()).GetSequencePoints()
            |> Seq.filter (fun (point: SequencePoint) -> not point.IsHidden && point.Offset <= offset)
            |> Seq.tryLast
            |> Option.map (fun (point: SequencePoint) ->
                let file: string = pdb.GetString(pdb.GetDocument(point.Document).Name)
                $"%s{Path.GetFileName file}:%i{point.StartLine}"
            )
        )

    [
        for handle in metadata.MethodDefinitions do
            let method: MethodDefinition = metadata.GetMethodDefinition handle

            if method.RelativeVirtualAddress <> 0 then
                let il: byte array = pe.GetMethodBody(method.RelativeVirtualAddress).GetILBytes()

                let mutable at: int = 0

                while at < il.Length do
                    let start: int = at

                    let value: int =
                        if il.[at] = 0xFEuy then
                            0xFE00 ||| int il.[at + 1]
                        else
                            int il.[at]

                    at <- at + (if value > 0xFF then 2 else 1)
                    let opCode: OpCode = opCodes.[value]

                    if
                        (opCode = OpCodes.Newobj || opCode = OpCodes.Call)
                        && buildsPrintfFormat metadata (BitConverter.ToInt32(il, at))
                    then
                        yield
                            {
                                Function = functionOf metadata method
                                MethodName = metadata.GetString method.Name
                                Location = locationOf handle start
                            }

                    at <- at + operandSize opCode il at
    ]
    |> List.distinct

/// The printf uses the tool can never reach, each with the reason it cannot.
let private unreachable: Map<string, string> =
    Map.ofList
        [
            "Triage.dump", "Dumps a node for triage inside a try, and falls back to the type name where printf fails."
            "CodePrinter.genNode", "The writer event payloads only exist for CodeFormatter.GetWriterEventsAsync."
            "TriviaNode.ToString", "Debugger display."
            "SingleTextNode.ToString", "Debugger display."
        ]

/// What the compiler writes for every record and union: a `ToString` and a debugger display that
/// go through `%+A`, and carry no line of source. They only run if something formats the value.
let private isGeneratedDisplay (printfUse: PrintfUse) : bool =
    printfUse.Location.IsNone
    && (printfUse.MethodName = "ToString" || printfUse.MethodName = "__DebugDisplay")

[<Test>]
let ``nothing the tool runs goes through printf`` () =
    let assemblies: Assembly list =
        [
            typeof<Fantomas.Daemon.FantomasDaemon>.Assembly
            typeof<Fantomas.Core.CodeFormatter>.Assembly
        ]

    let reported: string list =
        [
            for assembly in assemblies do
                for printfUse in printfUses assembly do
                    if not (isGeneratedDisplay printfUse || unreachable.ContainsKey printfUse.Function) then
                        let location: string = defaultArg printfUse.Location "no line"
                        yield $"%s{location} in %s{printfUse.Function}"
        ]
        |> List.sort

    Assert.That(
        reported,
        Is.Empty,
        "printf needs runtime code generation, which a Native AOT build does not have. Use concatenation, an interpolated string without %A, %O, width or precision, or ToString."
    )

// A scan that finds nothing because it reads nothing would pass the test above forever.
[<Test>]
let ``the printf scan finds what it is looking for`` () =
    let functions: string list =
        printfUses typeof<Fantomas.Core.CodeFormatter>.Assembly
        |> List.map (fun (printfUse: PrintfUse) -> printfUse.Function)

    Assert.That(functions, Does.Contain "Triage.dump")
