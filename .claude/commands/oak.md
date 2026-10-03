---
description: Parse F# source code to Oak (Fantomas intermediate representation)
allowed-tools: Bash(dotnet fsi:*), Bash(echo:*), Bash(dotnet build:*)
---

First build the project: `dotnet build src/Fantomas.Core.SnapshotTests` (the scripts reference its debug build, and it builds Fantomas.Core and Fantomas.EditorConfig too)

Then run the Oak script. Pass a file path as argument:

```
dotnet fsi scripts/oak.fsx [--signature] [--define FOO,BAR] <file>
```

Or pipe inline source via stdin:

```
echo '<source>' | dotnet fsi scripts/oak.fsx [--signature] [--define FOO,BAR]
```

$ARGUMENTS
