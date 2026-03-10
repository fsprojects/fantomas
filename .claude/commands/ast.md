---
description: Parse F# source code to untyped AST
allowed-tools: Bash(dotnet fsi:*), Bash(echo:*)
---

First build the project: `dotnet build src/Fantomas.Core/Fantomas.Core.fsproj`

Then run the AST script. Write the source code to a temp file and pass it as argument:

```
dotnet fsi scripts/ast.fsx [--signature] [--define FOO,BAR] <file>
```

Or pipe inline source via stdin:

```
echo '<source>' | dotnet fsi scripts/ast.fsx [--signature] [--define FOO,BAR]
```

$ARGUMENTS
