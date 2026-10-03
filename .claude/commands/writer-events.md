---
description: Show writer events produced during formatting of F# source code
allowed-tools: Bash(dotnet fsi:*), Bash(echo:*), Bash(dotnet build:*)
---

First build the project: `dotnet build src/Fantomas.Core.SnapshotTests` (the scripts reference its debug build, and it builds Fantomas.Core and Fantomas.EditorConfig too)

Then run the writer-events script. Pass a file path as argument:

```
dotnet fsi scripts/writer-events.fsx [--editorconfig <content>] [--define FOO,BAR] <file>
```

Or pipe inline source via stdin:

```
echo '<source>' | dotnet fsi scripts/writer-events.fsx [--editorconfig <content>] [--define FOO,BAR] [--signature]
```

$ARGUMENTS
