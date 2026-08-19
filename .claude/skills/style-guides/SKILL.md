---
name: style-guides
description: Look up what the F# style guides say about a formatting question. Use when deciding or defending a layout rule in Fantomas, when a Chains.md or design discussion needs a citation, or when the user asks what Microsoft or G-Research prescribe.
---

# F# style guides

Fantomas does not decide F# style, it implements it. Two documents are the authority, and a
layout decision that contradicts them needs a reason written down.

## The sources

- **Microsoft**, the default style Fantomas follows:
  <https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting>
  Fetch the raw markdown instead of the rendered page when you want to quote it:
  <https://raw.githubusercontent.com/dotnet/docs/main/docs/fsharp/style-guide/formatting.md>
- **G-Research**, the alternative style behind the `fsharp_*` G-Research settings:
  <https://github.com/G-Research/fsharp-formatting-conventions>
  Raw: <https://raw.githubusercontent.com/G-Research/fsharp-formatting-conventions/master/README.md>

Style discussions themselves happen at <https://github.com/fsharp/fslang-design#style-guide>, not
in this repository. See `docs/docs/end-users/StyleGuide.md`.

## How to use them

The Microsoft page is large. Fetching it whole returns tens of kilobytes, so pull the section you
need rather than the page. Its headings are stable and worth knowing:

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

## What to do with the answer

- Cite the guide in the design note or the doc change, with the rule quoted, so the next person
  does not have to re-derive it.
- When both guides agree, say so; that is a strong signal.
- When Fantomas disagrees with the guide, that is a bug report or a proposal to fslang-design, not
  a local preference to encode quietly.
