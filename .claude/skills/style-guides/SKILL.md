---
name: style-guides
description: Look up what the F# style guides say about a formatting question. Use when deciding or defending a layout rule in Fantomas, when a Chains.md or design discussion needs a citation, or when the user asks what Microsoft or G-Research prescribe.
---

# F# style guides

Fantomas does not decide F# style, it implements it. Two documents are the authority, and a
layout decision that contradicts them needs a reason written down.

## Always work from the local copies

Both guides belong in `.deps/style-guides/`, which is gitignored, so keeping them costs nothing and
leaves no trace. Never answer from the rendered pages or from memory: grep the local file and quote
it. Local copies are also what makes the examples testable, since you can run them through
Fantomas.

Refresh them at the start of the task. Both raw URLs return an `ETag`, so a conditional request
downloads only when the document changed and is free otherwise:

```bash
mkdir -p .deps/style-guides
cd .deps/style-guides
fetch() { curl -sS --etag-compare "$2.etag" --etag-save "$2.etag" -o "$2" "$1"; }
fetch https://raw.githubusercontent.com/dotnet/docs/main/docs/fsharp/style-guide/formatting.md microsoft-formatting.md
fetch https://raw.githubusercontent.com/G-Research/fsharp-formatting-conventions/master/README.md g-research-conventions.md
```

The `.etag` files next to the documents are what make the second run free; keep them. Verified that
a repeat run leaves the file byte for byte intact rather than truncating it.

## The sources

- **Microsoft**, the default style Fantomas follows.
  Local: `.deps/style-guides/microsoft-formatting.md`
  Rendered: <https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting>
- **G-Research**, the alternative style behind the G-Research settings.
  Local: `.deps/style-guides/g-research-conventions.md`
  Rendered: <https://github.com/G-Research/fsharp-formatting-conventions>

Style discussions themselves happen at <https://github.com/fsharp/fslang-design#style-guide>, not
in this repository. See `docs/docs/end-users/StyleGuide.md`.

## How to use them

The Microsoft document is large, tens of kilobytes, so grep the local copy for the section you need
rather than reading it whole. Its headings are stable and worth knowing:

- `Formatting application expressions`
- `Formatting lambda expressions`
- `Formatting function and member arguments`
- `Formatting pipeline expressions`
- `Formatting if expressions`
- `Formatting record expressions`

Quote verbatim, including the ✔️ and ❌ examples. The ❌ ones carry the reasoning, and the reason
is usually the part that settles an argument. One example: the guide rejects lambda parameters
aligned under an opening parenthesis, because the column then depends on the length of the
identifier in front of it.

## Checking our output against the examples

The code blocks are testable: extract them from the local copy and run them through Fantomas with
the settings that style implies. Three traps make a naive pass-rate meaningless, all seen for real:

- Some blocks mark the bad version by **variable name**, `let bad = ...`, rather than by a
  `// Not OK` comment, so Fantomas rewriting it is the guide being obeyed, not broken.
- Many blocks are **fragments** that do not parse alone, or illustrate naming and structure rather
  than layout. `if cond then e1 else e2` shown across four lines is not a claim about line breaks.
- The **settings have to be right**, and are best read off the guide's own examples rather than off
  our documentation: the G-Research examples use `aligned` brackets and a space before the colon in
  record fields, which does not match every badge in `docs/docs/end-users/Configuration.fsx`.

So classify a block before diffing it, and say what was excluded and why. A pass rate without that
classification says nothing.

## What to do with the answer

- Cite the guide in the design note or the doc change, with the rule quoted, so the next person
  does not have to re-derive it.
- When both guides agree, say so; that is a strong signal.
- When Fantomas disagrees with the guide, that is a bug report or a proposal to fslang-design, not
  a local preference to encode quietly.
