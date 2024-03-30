(**
---
category: End-users
categoryindex: 1
index: 12
---
# Recipes

Fantomas has a limited set of settings and adheres to style guides.
These style guides are meant to be agnostic to any specific framework.

That being said, there are certain combinations that provide a more ideal result when working with certain scenarios.

**These snippets aren’t to spark requests for new settings suited to framework of the day!**

Remember that a `.editorconfig` file can apply certain settings only to certain files.
Sometimes, it makes sense to tweak a few setting for a subset of your codebase.

*)

(*** hide ***)
#r "../../../artifacts/bin/Fantomas.FCS/release/Fantomas.FCS.dll"
#r "../../../artifacts/bin/Fantomas.Core/release/Fantomas.Core.dll"
#r "../../../artifacts/bin/Fantomas/release/EditorConfig.Core.dll"
#load "../../../src/Fantomas/EditorConfig.fs"

open System
open Fantomas.Core
open Fantomas.EditorConfig

let formatCode input (settings: string) =
    async {
        let config =
            let editorConfigProperties =
                settings.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                |> Array.choose (fun line ->
                    let parts = line.Split('=', StringSplitOptions.TrimEntries)

                    if parts.Length <> 2 then
                        None
                    else
                        Some(parts.[0], parts.[1]))
                |> readOnlyDict

            parseOptionsFromEditorConfig FormatConfig.Default editorConfigProperties

        let! result = CodeFormatter.FormatDocumentAsync(false, input, config)
        printf $"%s{result.Code}"
    }
    |> Async.RunSynchronously

(**
## HTML DSL

When working with a HTML inspired DSL (typically a function call with one or two lists), you can use [fsharp_experimental_elmish](https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html#fsharp_experimental_elmish)

### Fable.React
### Elmish
*)

formatCode
    """
div
    []
    [
        h1 [] [ str "Some title" ]
        ul
            []
            [
                for p in model.Points do
                    li [] [ str $"%i{p.X}, %i{p.Y}" ]
            ]
        hr []
    ]
    """
    """
fsharp_experimental_elmish = true
    """
(*** include-output ***)

(**
### Feliz
*)

formatCode
    """
Html.div
    [
        Html.h1 [ str "Some title" ]
        Html.ul
            [
                for p in model.Points do
                    Html.li [ str $"%i{p.X}, %i{p.Y}" ]
            ]
    ]
    """
    """
fsharp_experimental_elmish = true
    """
(*** include-output ***)

(**
### Giraffe
*)

formatCode
    """
let indexView =
    html
        []
        [ head [] [ title [] [ str "Giraffe Sample" ] ]
          body
              []
              [ h1 [] [ str "I |> F#" ]
                p [ _class "some-css-class"; _id "someId" ] [ str "Hello World" ] ] ]
    """
    """
fsharp_experimental_elmish = true
    """
(*** include-output ***)

(**
### Falco
*)

formatCode
    """
let markup =
    Elem.div
        []
        [ Text.comment "An HTML comment"
          Elem.p [] [ Text.raw "A paragraph" ]
          Elem.p [] [ Text.rawf "Hello %s" "Jim" ]
          Elem.code [] [ Text.enc "<div>Hello</div>" ] ] // HTML encodes text before rendering
    """
    """
fsharp_experimental_elmish = true
    """
(*** include-output ***)

(**
## Expecto

The [Expect](https://github.com/haf/expecto) test framework can benefit from [Stroustrup](https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html#fsharp_multiline_bracket_style) style when defining test lists.
*)

formatCode
    """
let tests =
    testList
        "A test group"
        [ test "one test" { Expect.equal (2 + 2) 4 "2+2" }

          test "another test that fails" { Expect.equal (3 + 3) 5 "3+3" }

          testAsync "this is an async test" {
              let! x = async { return 4 }
              Expect.equal x (2 + 2) "2+2"
          }

          testTask "this is a task test" {
              let! n = Task.FromResult 2
              Expect.equal n 2 "n=2"
          } ]
    |> testLabel "samples"
    """
    """
fsharp_multiline_bracket_style = stroustrup
    """
(*** include-output ***)

(**
## FAKE

At the end of a [FAKE](https://fake.build/) script, the target dependencies are typically listed using custom operators.  
Using [fsharp_max_infix_operator_expression](https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html#fsharp_max_infix_operator_expression)
you can tweak when they should go to the next line.
*)

formatCode
    """
"Build"
==> "EnsureCanScaffoldCodeFix"
==> "LspTest"
==> "Coverage"
==> "Test"
==> "All"

"Clean" ==> "LocalRelease" ==> "ReleaseArchive" ==> "Release"
    """
    """
fsharp_max_infix_operator_expression = 5
    """
(*** include-output ***)

(**

## Paragraphs

When expressions are multiline Fantomas will put a blank line before and after them.
This helps for code to stay consistent in teams. 
*)

formatCode
    """
let Foo =
    try // Start paragraph 1
        printfn "%A" blah
    with ex ->
        printfn "Failed to print blah" // End paragraph 1

    let mutable a = 8 // This line belongs // Start paragraph 2
    for i in [ 1 .. 10 ] do
        a <- a + i // Start paragraph 2
    """
    ""
(*** include-output ***)

(**
However, some developers like to group their code in self-defined paragraphs, and thus want full control when Fantomas inserts blank lines.
Full control is not possible but outside top level expressions this can be done via [fsharp_blank_lines_around_nested_multiline_expressions = false](https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html#fsharp_blank_lines_around_nested_multiline_expressions).

⚠️ This approach is generally advised against because it gives the code author control over newlines. 
Within teams, this can lead to debates, which is precisely what using Fantomas aims to eliminate: code style discussions. ⚠️

*)

formatCode
    """
let Foo =
    try // Start paragraph 1
        printfn "%A" blah
    with ex ->
        printfn "Failed to print blah" // End paragraph 1

    let mutable a = 8 // This line belongs // Start paragraph 2
    for i in [ 1 .. 10 ] do
        a <- a + i // Start paragraph 2
    """
    """
fsharp_blank_lines_around_nested_multiline_expressions = false
    """
(*** include-output ***)

(**
## Early returns

In F# there is no such concept as early returns. However, sometimes we do want to return an empty or error result when a certain condition is not met.
To avoid extreme indentation in our happy path, you can use [fsharp_experimental_keep_indent_in_branch = true](https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html#fsharp_experimental_keep_indent_in_branch).
*)

formatCode
    """
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
    Run.ifDiagnosticByCode (set [ "20" ]) (fun diagnostic (codeActionParams: CodeActionParams) ->
        asyncResult {
            if mDiag.StartLine <> mDiag.EndLine then
                // Only do single line for now
                return []
            else

            // Happy path at same indent

            let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
                getParseResultsForFile fileName fcsPos

            let mExprOpt =
                (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
                ||> ParsedInput.tryPick (fun path node ->
                    match node with
                    | SyntaxNode.SynExpr(e) when Range.equals mDiag e.Range -> Some(path, e)
                    | _ -> None)

            match mExprOpt with
            | None ->
                // Empty result is Option is None
                return []
            | Some(path, expr) ->

            // Happy path at same indent

            return
                [ { SourceDiagnostic = None
                    Title = title
                    File = codeActionParams.TextDocument
                    Edits =
                        [| { Range = fcsRangeToLsp expr.Range
                            NewText = newText } |]
                    Kind = FixKind.Fix } ]
        })

    """
    """
fsharp_experimental_keep_indent_in_branch = true
    """
(*** include-output ***)

(**
⚠️ The downside of this setting is that it only respects this style of formatting if it was already present in the original source.
The problem with this approach is that the author of the original code decides whether this style is used.
Discuss this with your team! ⚠️

*)
