module Fantomas.FormatConfig

open System

type FormatException(msg : string) =
    inherit Exception(msg)

type Num = int

type FormatConfig = 
    { /// Number of spaces for each indentation
      IndentSpaceNum : Num;
      /// The column where we break to new lines
      PageWidth : Num;
      PreserveEndOfLine : bool;
      SemicolonAtEndOfLine : bool;
      SpaceBeforeArgument : bool;
      SpaceBeforeColon : bool;
      SpaceAfterComma : bool;
      SpaceAfterSemicolon : bool;
      IndentOnTryWith : bool;
      /// Reordering and deduplicating open statements
      ReorderOpenDeclaration : bool;
      SpaceAroundDelimiter : bool;
      /// Prettyprinting based on ASTs only
      StrictMode : bool }

    static member Default = 
        { IndentSpaceNum = 4; PageWidth = 80;
          PreserveEndOfLine = false;
          SemicolonAtEndOfLine = false; SpaceBeforeArgument = true; SpaceBeforeColon = true;
          SpaceAfterComma = true; SpaceAfterSemicolon = true; 
          IndentOnTryWith = false; ReorderOpenDeclaration = false; 
          SpaceAroundDelimiter = true; StrictMode = false }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration, spaceAroundDelimiter) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration;
              SpaceAroundDelimiter = spaceAroundDelimiter }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration, 
                         spaceAroundDelimiter, strictMode) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration;
              SpaceAroundDelimiter = spaceAroundDelimiter;
              StrictMode = strictMode }
