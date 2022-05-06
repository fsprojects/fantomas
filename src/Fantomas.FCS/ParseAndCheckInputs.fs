module internal FSharp.Compiler.ParseAndCheckInputs

open Internal.Utilities.Library
open FSharp.Compiler.IO

let FSharpSigFileSuffixes = [ ".mli"; ".fsi" ]

let mlCompatSuffixes = [ ".mli"; ".ml" ]

let FSharpImplFileSuffixes = [ ".ml"; ".fs"; ".fsscript"; ".fsx" ]

let FSharpScriptFileSuffixes = [ ".fsscript"; ".fsx" ]


let CanonicalizeFilename filename =
    let basic = FileSystemUtils.fileNameOfPath filename

    String.capitalize (
        try
            FileSystemUtils.chopExtension basic
        with
        | _ -> basic
    )

let IsScript filename =
    let lower = String.lowercase filename

    FSharpScriptFileSuffixes
    |> List.exists (FileSystemUtils.checkSuffix lower)
