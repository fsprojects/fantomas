---
category: End-users
categoryindex: 1
index: 15
---

# Formatting from an editor with Fantomas.Client

The [Fantomas.Client](https://www.nuget.org/packages/Fantomas.Client) NuGet package is for tools that
format someone else's code: editor extensions, language servers, custom build tooling.

It exists because of a versioning problem. If your editor extension referenced
[Fantomas.Core](https://www.nuget.org/packages/Fantomas.Core) directly, everyone using that extension
would be formatting with whatever version you happened to compile against. A repository that pins
Fantomas 6 in its `dotnet-tools.json` would silently be formatted by Fantomas 8, and the diff would
be enormous.

`Fantomas.Client` avoids this. For each file you ask it to format, it looks for the Fantomas the
*user* installed, starts that version as a background daemon, and talks to it over JSON-RPC. Your
tool stays on one version of `Fantomas.Client`, while the formatting is done by the version the
repository asked for.

> The code on this page is illustrative rather than runnable. It is meant to show the shape of the
API, not to be pasted into a script.

## Formatting a document

```fsharp
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService

// One service for the lifetime of your tool. It caches a daemon per Fantomas version, mapping each
// folder to the version it resolved, so two folders pinning the same version share one process.
// Creating a service per request would start a new one every time.
let service: FantomasService = new LSPFantomasService()

let request =
    { SourceCode = "let a  =  1"
      // Must be absolute, and must exist on disk. Its folder is what decides which Fantomas
      // version gets used, so a path inside the user's repository is what you want here.
      FilePath = "/home/me/MyProject/Library.fs"
      // None means "use the .editorconfig that applies to FilePath".
      Config = None
      Cursor = None }

let response = service.FormatDocumentAsync(request).Result

if response.Code = int FantomasResponseCode.Formatted then
    // Content holds the formatted code.
    printfn "%s" (Option.defaultValue "" response.Content)
elif response.Code = int FantomasResponseCode.UnChanged then
    // The file was already formatted. Content is None, so do not overwrite anything.
    ()
else
    // Error, Ignored, ToolNotFound, FileNotFound, FilePathIsNotAbsolute, DaemonCreationFailed, ...
    // Content carries a message explaining what went wrong.
    eprintfn "%s" (Option.defaultValue "" response.Content)
```

Every call answers with the same `FantomasResponse`, and `Code` is what you branch on. It is an
`int` rather than a union so the type survives the wire; compare it against `FantomasResponseCode`.
Nothing throws for an unusable path or a missing tool, so there is exactly one place to handle
failure.

## Reacting to a bad `.editorconfig`

A `.editorconfig` can name a setting Fantomas does not have, usually a typo such as
`fsharp_multiline_brackets_style`, or give a setting a value it cannot parse. Formatting still
succeeds, using defaults for whatever could not be read, so without being told the user sees the
setting quietly not apply.

Subscribe to `ConfigurationWarnings` to surface it:

```fsharp
service.ConfigurationWarnings.Add(fun warning ->
    if Array.isEmpty warning.Problems then
        // Nothing is wrong any more, so clear whatever you showed for this file earlier.
        clearWarnings warning.FilePath
    else
        warning.Problems
        |> Array.map (fun problem ->
            if problem.Code = int ConfigurationProblemCode.UnknownSetting then
                $"%s{problem.Setting} is not a Fantomas setting"
            else
                $"%s{problem.Setting} does not accept the value %s{problem.Value}")
        |> showWarnings warning.FilePath warning.EditorConfigFiles)
```

Worth knowing:

* The event is raised for **every** format request, and before that request answers, with an
empty `Problems` array when the configuration is fine. That is what lets you clear a warning
once the user fixes it.
* You can have as many format requests in flight as you like. Warnings for one file arrive in the
order the daemon received them, because the daemon serves one request at a time per file, so an empty
one never overtakes problems that are still current. Requests for different files are served
concurrently and their warnings interleave; `FilePath` is what tells them apart.
* Nothing is coalesced. Several requests queued for one file each run to completion in turn, so a
tool that fires a burst of them will see latency grow with the length of the queue. None of them
is served stale input, because each request carries its own `SourceCode`, but if you format on
every keystroke you want to be dropping your own superseded requests rather than sending them.
* `EditorConfigFiles` holds the absolute paths of the `.editorconfig` files that contributed. Which
one a given problem came from is not knowable, because editorconfig merges the whole chain into a
single set of properties before Fantomas sees it. There is no line number either, so name the
setting rather than trying to point at it.
* `Source` tells you whether the setting came from a `.editorconfig` on disk or from the `Config`
dictionary your own tool sent along with the request.
* Only Fantomas 8 daemons send these. Against an older one the event simply never fires, so no
version check is needed.
* The event is raised on whichever thread the daemon's message arrived on, never on the thread that
asked for the formatting. Marshal before touching a UI. A handler that throws is swallowed rather
than allowed to fault the connection, so nothing is lost but nothing is reported either.

If you are talking to the daemon yourself rather than through `Fantomas.Client`, the notification
arrives on `fantomas/configurationWarning` carrying one object. The shape of that object is below.
The framing around it is StreamJsonRpc's default, which is JSON-RPC with `Content-Length` headers,
the same as the Language Server Protocol uses, so this is what the payload looks like and not what
goes on the wire byte for byte:

```json
{
  "FilePath": "/home/me/MyProject/Library.fs",
  "EditorConfigFiles": ["/home/me/MyProject/.editorconfig"],
  "Problems": [
    { "Code": 1, "Source": 1, "Setting": "fsharp_multiline_brackets_style", "Value": null },
    { "Code": 2, "Source": 2, "Setting": "fsharp_experimental_elmish", "Value": "not_a_bool" }
  ]
}
```

`Code` is `1` for a setting Fantomas does not have and `2` for a value it cannot parse; `Source` is
`1` for a `.editorconfig` on disk and `2` for the `Config` dictionary sent with the request.
`Value` is `null` for `Code` `1`, because no value was ever read. `EditorConfigFiles` is empty when
`Problems` is empty.

## Formatting a selection

```fsharp
let selectionRequest =
    { SourceCode = sourceCode
      FilePath = "/home/me/MyProject/Library.fs"
      Config = None
      // Same semantics as the F# compiler's range: one-based lines, zero-based columns.
      Range = FormatSelectionRange(1, 0, 3, 12) }

let selectionResponse = service.FormatSelectionAsync(selectionRequest).Result
```

The range that actually got formatted comes back in `SelectedRange`, which can differ from what you
asked for when the selection had leading or trailing whitespace. Use the returned range when you
splice the result back into the document, not the one you sent.

## Keeping the cursor in place

Pass `Cursor` on a format request and the response tells you where that position ended up after
formatting, so the caret does not jump when you format on save.

```fsharp
let request =
    { SourceCode = sourceCode
      FilePath = "/home/me/MyProject/Library.fs"
      Config = None
      Cursor = Some(FormatCursorPosition(4, 2)) }
```

## Discovering settings

`ConfigurationAsync` returns the settings schema of the Fantomas version resolved for a file: every
setting name, its type, default, and description. Because it comes from the daemon rather than from
whatever you compiled against, it is the authoritative list for that repository, and it is what you
want backing a settings UI or a `.editorconfig` completion list.

```fsharp
let schema = service.ConfigurationAsync("/home/me/MyProject/Library.fs").Result
// schema.Content holds JSON describing every setting.
```

## Lifetime

```fsharp
// Stop every daemon this service started. Call it when your tool shuts down.
service.Dispose()
```

`ClearCache` throws away the daemons without disposing the service, which is what you want after the
user changes the Fantomas version in their `dotnet-tools.json`. Otherwise the old process keeps
serving requests for the rest of the session.
