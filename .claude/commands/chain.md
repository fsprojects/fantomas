---
description: Show the ExprChain structure (head, segments, terminal) of F# source code
allowed-tools: Bash(dotnet fsi:*), Bash(echo:*), Bash(dotnet build:*)
---

First build the project: `dotnet build src/Fantomas.Core/Fantomas.Core.fsproj`

Then run the chain script. Pass a file path as argument:

```
dotnet fsi scripts/chain.fsx [--signature] <file>
```

Or pipe inline source via stdin:

```
echo '<source>' | dotnet fsi scripts/chain.fsx [--signature]
```

$ARGUMENTS
