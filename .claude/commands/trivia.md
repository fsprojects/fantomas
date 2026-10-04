---
description: Show where each piece of trivia landed in F# source code (node, token, side and kind)
allowed-tools: Bash(dotnet fsi:*), Bash(echo:*), Bash(dotnet build:*)
---

First build the project: `dotnet build src/Fantomas.Core.SnapshotTests` (the scripts reference its debug build, and it builds Fantomas.Core and Fantomas.EditorConfig too)

Then run the trivia script. Pass a file path as argument:

```
dotnet fsi scripts/trivia.fsx [--signature] [--editorconfig <content>] <file>
```

Or pipe inline source via stdin:

```
echo '<source>' | dotnet fsi scripts/trivia.fsx [--editorconfig <content>] [--signature]
```

$ARGUMENTS
