(*---
# Fields that do not fit go one per line, indented below `of`.
---*)
exception ProcessFailedWithExitCode of exitCode: int * standardOutput: string * standardError: string * commandLine: string
