module Fantomas.Utils

open Microsoft.FSharp.Compiler.Range

type Position = { Line : int; Column : int }
type SrcLoc = { FileName : string; StartPos : Position; EndPos : Position }     

/// Create a source location
let mkSrcLoc (r : range)  = 
    { FileName = r.FileName
      StartPos = { Line = r.StartLine; Column = r.StartColumn }
      EndPos = { Line = r.EndLine; Column = r.EndColumn } }

/// Merge two source locations
let joinSrcLoc { FileName = fileName1; StartPos = start1 } { EndPos = end2 } =
    { FileName = fileName1; StartPos = start1 ; EndPos = end2 }