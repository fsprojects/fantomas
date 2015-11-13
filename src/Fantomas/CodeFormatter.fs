namespace Fantomas

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

[<Sealed>]
type CodeFormatter =
    static member FormatDocumentAsync(fileName, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatDocument config
    
    static member FormatDocument(fileName, source, config) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.formatDocument config
        |> Async.RunSynchronously
   
    static member FormatSelectionAsync(fileName, selection, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatSelection selection config
    
    static member FormatSelection(fileName, selection, source, config) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.formatSelection selection config
        |> Async.RunSynchronously

    static member FormatAroundCursorAsync(fileName, cursorPos, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatAroundCursor cursorPos config
    
    static member FormatAroundCursor(fileName, cursorPos, source, config) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.formatAroundCursor cursorPos config
        |> Async.RunSynchronously
    
    static member internal FormatSelectionInDocument(fileName, selection, source, config) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.formatSelectionInDocument selection config
        |> Async.RunSynchronously
    

    static member FormatAST(ast, source, config) = 
        CodeFormatterImpl.formatAST ast source config

    static member ParseAsync(fileName, source, projectOptions, checker) = 
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.parse
    
    static member Parse(fileName, source) = 
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.parse
        |> Async.RunSynchronously

    static member IsValidAST ast = 
        CodeFormatterImpl.isValidAST ast

    static member IsValidFSharpCodeAsync(fileName, source, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.isValidFSharpCode

    static member IsValidFSharpCode(fileName, source) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.isValidFSharpCode
        |> Async.RunSynchronously

    static member MakePos(line, col) = 
        CodeFormatterImpl.makePos line col

    static member MakeRange(startLine, startCol, endLine, endCol) = 
        CodeFormatterImpl.makeRange startLine startCol endLine endCol

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CodeFormatter =
    let internal createFormatContextNoFileName isFsiFile sourceCode =
        let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
        CodeFormatterImpl.createFormatContextNoChecker fileName sourceCode

    let parse isFsiFile sourceCode = 
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.parse
        |> Async.RunSynchronously

    let isValidAST ast = 
        CodeFormatterImpl.isValidAST ast

    let isValidFSharpCode isFsiFile sourceCode =
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.isValidFSharpCode
        |> Async.RunSynchronously

    let formatSourceString isFsiFile sourceCode config =
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.formatDocument config
        |> Async.RunSynchronously

    let formatAST ast sourceCode config = 
        CodeFormatterImpl.formatAST ast sourceCode config
 
    let makeRange startLine startCol endLine endCol = 
        CodeFormatterImpl.makeRange startLine startCol endLine endCol

    let formatSelectionOnly isFsiFile (range : range) (sourceCode : string) config =
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.formatSelection range config
        |> Async.RunSynchronously

    let formatSelectionExpanded isFsiFile (range : range) (sourceCode : string) config =
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.formatSelectionExpanded range config
        |> Async.RunSynchronously

    let formatSelectionFromString isFsiFile (range : range) (sourceCode : string) config =
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.formatSelectionInDocument range config
        |> Async.RunSynchronously

    let makePos line col = 
        CodeFormatterImpl.makePos line col

    let formatAroundCursor isFsiFile (cursorPos : pos) (sourceCode : string) config = 
        createFormatContextNoFileName isFsiFile sourceCode
        |> CodeFormatterImpl.formatAroundCursor cursorPos config
        |> Async.RunSynchronously
