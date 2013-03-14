

// Options:
//  --encoding=<encoding>               Set the encoding, e.g. UTF-8. If not set, defaults to the platform default encoding (currently UTF-8).
//  --force, -f                         If using --stdout, print the source unchanged if it cannot be parsed correctly.
//  --help, -h                          Show help
//  --config=<path>, -c=<path>          Read preferences from a config file
//  --recurse, -r                       If any given file is a directory, recurse beneath it and collect all fs/fsx/fsi files for processing
//  --stdin                             Read F# source from standard input
//  --stdout                            Write the formatted output to standard output

// Preferences:
//  -indentSpaceNum=[1-10]              Set number of spaces to use for indentation
//  -longIdentLength=[10-100]           The length to start breaking an expression to multiple lines
//  [+|-]semicolonAtEndOfLine           Enable/disable semicolons at the end of line (default = false)
//  [+|-]spaceBeforeArgument            Enable/disable spaces before the first argument (default = false)
//  [+|-]spaceBeforeColon               Enable/disable spaces before colons (default = true)
//  [+|-]spaceAfterComma                Enable/disable spaces after commas (default = true)
//  [+|-]spaceAfterSemiColon            Enable/disable spaces after semicolons (default = true)
//  [+|-]indentOnTryWith                Enable/disable indentation on try/with block (default = false)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0
