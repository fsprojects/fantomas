module FantomasTools.Client.ASTViewer.Decoders

open ASTViewer.Shared
open FantomasTools.Client.ASTViewer.Model
open Thoth.Json

let decodeKeyValue: Decoder<obj> = fun _key jsonValue -> Ok jsonValue

#nowarn "40"
