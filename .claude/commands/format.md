---
description: Format F# source code using the locally built Fantomas
allowed-tools: Bash(dotnet fsi:*), Bash(echo:*), Bash(dotnet build:*)
---

First build the project: `dotnet build src/Fantomas.Core.SnapshotTests` (the scripts reference its debug build, and it builds Fantomas.Core and Fantomas.EditorConfig too)

Then run the format script. Pass a file path as argument:

```
dotnet fsi scripts/format.fsx [--editorconfig <content>] <file>
```

Or pipe inline source via stdin:

```
echo '<source>' | dotnet fsi scripts/format.fsx [--editorconfig <content>] [--signature]
```

$ARGUMENTS
