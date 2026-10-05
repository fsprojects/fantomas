(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>
        ((fun _ ->
            fragment []
                [ h3 []
                      [ str "Heading "
                        Badge.badge [ Badge.Color Secondary ] [ str "New" ] ]
                  Badge.badge [ Badge.Color Warning ] [ str "oh my" ]]), "BadgeSample")

exportDefault badgeSample
