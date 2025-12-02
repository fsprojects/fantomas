#r "nuget: Thoth.Json.System.Text.Json"
#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"

open System.Threading.Tasks
open Fantomas.Core
open Thoth.Json.Core
open Thoth.Json.System.Text.Json

let oakTypes =
    let a = typeof<SyntaxOak.Oak>.Assembly
    let allTypes = a.GetExportedTypes()

    allTypes
    |> Array.choose (fun t ->
        if not (t.FullName.Contains("Fantomas.Core.SyntaxOak+")) then
            None
        else
            Some(t.FullName, t))
    |> Map.ofArray

let rec simplifyTypeName (t: System.Type) : string =
    if t.IsArray then
        let elementType = t.GetElementType()
        $"{simplifyTypeName elementType} array"
    elif t.IsGenericType then
        let genericDef = t.GetGenericTypeDefinition()
        let typeArgs = t.GetGenericArguments() |> Array.map simplifyTypeName

        let baseName = genericDef.FullName

        if isNull baseName then
            t.Name
        elif baseName = "System.Collections.Generic.IEnumerable`1" then
            $"{typeArgs[0]} seq"
        elif baseName = "Microsoft.FSharp.Core.FSharpOption`1" then
            $"{typeArgs[0]} option"
        elif baseName = "Microsoft.FSharp.Collections.FSharpList`1" then
            $"{typeArgs[0]} list"
        else
            // For other generic types, show name with type parameters
            let nameWithoutArity = genericDef.Name.Split('`')[0]
            let typeParams = String.concat ", " (Array.toList typeArgs)
            $"{nameWithoutArity}<{typeParams}>"
    // For non-generic types, just use the name
    elif isNull t.FullName then
        t.Name
    else
        // Extract just the type name, removing namespace if it's SyntaxOak
        let fullName = t.FullName

        if fullName.StartsWith("Fantomas.Core.SyntaxOak+") then
            fullName.Replace("Fantomas.Core.SyntaxOak+", "")
        else
            t.Name

let encodeProperty (prop: System.Reflection.PropertyInfo) =
    let typeName = simplifyTypeName prop.PropertyType

    Encode.object [ "name", Encode.string prop.Name; "type", Encode.string typeName ]

let encodeType (t: System.Type) =
    // Detect if this is a DU case: nested types (e.g., TypeDefn+Abbrev) are DU cases
    let isDucase =
        let fullName = t.FullName

        if isNull fullName then
            false
        else
            // Count '+' after the namespace - nested types have multiple '+'
            let parts = fullName.Split('+')
            parts.Length > 2 // Namespace + ParentType + CaseName

    // Detect if this is a DU type (like Expr, Type) - has Tag property and many IsXYZ properties
    let isDuType =
        if isDucase then
            false
        else
            let props =
                t.GetProperties(
                    System.Reflection.BindingFlags.Public
                    ||| System.Reflection.BindingFlags.Instance
                )

            let hasTag =
                props |> Array.exists (fun p -> p.Name = "Tag" && p.PropertyType = typeof<int>)

            let isXxxCount =
                props
                |> Array.filter (fun p -> p.PropertyType = typeof<bool> && p.Name.StartsWith("Is"))
                |> Array.length

            hasTag && isXxxCount > 5 // DU types have many IsXYZ properties

    // Find all cases for DU types
    let duCases =
        if not isDuType then
            []
        else
            oakTypes
            |> Map.values
            |> Seq.filter (fun caseType ->
                let fullName = caseType.FullName

                if isNull fullName then
                    false
                else
                    let parts = fullName.Split('+')
                    // Case should be: Namespace + ParentType + CaseName
                    parts.Length = 3
                    &&
                    // Parent type should match
                    let parentTypeName = parts.[parts.Length - 2] in

                    parentTypeName = t.Name
                    &&
                    // Case should inherit from the DU type
                    (not (isNull caseType.BaseType))
                    && caseType.BaseType = t)
            |> Seq.map (fun caseType -> caseType.Name)
            |> Seq.sort
            |> Seq.toList

    let properties =
        t.GetProperties(
            System.Reflection.BindingFlags.Public
            ||| System.Reflection.BindingFlags.Instance
        )
        |> Array.filter (fun p ->
            // Skip indexers
            p.GetIndexParameters().Length = 0
            &&
            // Skip TryGetCursor - not relevant
            p.Name <> "TryGetCursor"
            &&
            // Filter out DU-related properties: IsXxx booleans and Tag for both DU types and cases
            (not (p.PropertyType = typeof<bool> && p.Name.StartsWith("Is")))
            && p.Name <> "Tag")
        |> Array.sortBy (fun p -> p.Name)

    let baseTypeEncoder =
        if isNull t.BaseType || t.BaseType = typeof<obj> then
            Encode.nil
        else
            Encode.string t.BaseType.Name

    let interfaces =
        t.GetInterfaces()
        |> Array.filter (fun i -> i.FullName.StartsWith("Fantomas.Core.SyntaxOak"))
        |> Array.map (fun i -> Encode.string i.Name)
        |> Array.toList

    let baseFields =
        [ "name", Encode.string t.Name
          "fullName", Encode.string t.FullName
          "baseType", baseTypeEncoder ]

    let interfaceField =
        if List.isEmpty interfaces then
            []
        else
            [ "interfaces", Encode.list interfaces ]

    // For DU types, include cases instead of properties
    let contentFields =
        if isDuType && not (List.isEmpty duCases) then
            [ "cases", Encode.list (List.map Encode.string duCases) ]
        else
            [ "properties", Encode.array (Array.map encodeProperty properties) ]

    Encode.object (baseFields @ interfaceField @ contentFields)

let findOakTypes (input: string list) : string =
    let inputLower = List.map (fun (s: string) -> s.ToLowerInvariant()) input

    oakTypes
    |> Map.filter (fun _name t ->
        let typeNameLower = t.Name.ToLowerInvariant()
        List.exists (fun (inputStr: string) -> typeNameLower = inputStr) inputLower)
    |> Map.toList
    |> List.map (fun (_name, t) -> encodeType t)
    |> Encode.list
    |> Encode.toString 2

// Temp for debugging
// oakTypes
// |> Map.values
// |> Seq.map (fun t -> encodeType t)
// |> Encode.seq
// |> Encode.toString 2
// |> printfn "%s"

open SyntaxOak

let encodeRange (m: Fantomas.FCS.Text.range) =
    Encode.string $"[{m.StartLine}:{m.StartColumn}--{m.EndLine}:{m.EndColumn}]"

let encodeTriviaNode (triviaNode: TriviaNode) : IEncodable =
    let contentType, content =
        match triviaNode.Content with
        | CommentOnSingleLine comment -> "commentOnSingleLine", Some comment
        | LineCommentAfterSourceCode comment -> "lineCommentAfterSourceCode", Some comment
        | BlockComment(comment, _, _) -> "blockComment", Some comment
        | Newline -> "newline", None
        | Directive directive -> "directive", Some directive
        | Cursor -> "cursor", None

    Encode.object
        [ yield "range", encodeRange triviaNode.Range
          yield "type", Encode.string contentType
          match content with
          | None -> ()
          | Some content -> yield "content", Encode.string content ]

let rec encodeNode (node: SyntaxOak.Node) (continuation: IEncodable -> IEncodable) : IEncodable =
    let continuations = List.map encodeNode (Array.toList node.Children)

    let text =
        match node with
        | :? SingleTextNode as stn ->
            if stn.Text.Length < 30 then
                stn.Text
            else
                sprintf "%s.." (stn.Text.Substring(0, 30))
            |> Some
        | _ -> None

    let finalContinuation (children: IEncodable list) =
        Encode.object
            [ yield "type", Encode.string (node.GetType().Name)
              match text with
              | None -> ()
              | Some text -> yield "text", Encode.string text
              yield "range", encodeRange node.Range
              if node.HasContentBefore then
                  yield ("contentBefore", Encode.seq (Seq.map encodeTriviaNode node.ContentBefore))
              if node.Children.Length > 0 then
                  yield ("children", Encode.list children)
              if node.HasContentAfter then
                  yield ("contentAfter", Encode.seq (Seq.map encodeTriviaNode node.ContentAfter)) ]
        |> continuation

    Continuation.sequence continuations finalContinuation

let parseOak (input: string) (isSignature: bool) : Task<string> =
    task {
        try
            let! oaks = CodeFormatter.ParseOakAsync(isSignature, input)

            match Array.tryHead oaks with
            | None -> return "No Oak found in input"
            | Some(oak, _) ->
                return
                    (encodeNode oak id //
                     |> Encode.toString 2)

        with ex ->
            return
                Encode.object
                    [ "message", Encode.string "Error while parsing to Oak"
                      "error", Encode.string (string ex) ]
                |> Encode.toString 2
    }

open System.IO

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample = File.ReadAllText(fsi.CommandLineArgs.[fsi.CommandLineArgs.Length - 1])
        let isSignature = sample.EndsWith(".fsi")

        parseOak sample isSignature
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi oak.fsx <input file>"
