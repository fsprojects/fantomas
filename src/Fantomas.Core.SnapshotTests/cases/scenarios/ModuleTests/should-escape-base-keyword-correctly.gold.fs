open System
open RDotNet
open RDotNet.NativeLibrary
open RDotNet.Internals
open RProvider
open RProvider.``base``
open RProvider.stats

[<EntryPoint>]
let main argv =
    let a = R.rnorm (1000)
    0
