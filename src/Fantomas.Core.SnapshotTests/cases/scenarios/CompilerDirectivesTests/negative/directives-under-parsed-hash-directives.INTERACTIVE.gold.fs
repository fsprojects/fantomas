#load "../../.paket/load/netstandard2.0/Client/client.group.fsx"
#load "../Overview/Types.fs"
#load "./Shared.fsx"
#load "../Overview/Types.fs"

#if INTERACTIVE
#r "netstandard"
#endif

open Fable.React
open Fable.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Teams.Overview.Types
open Browser.Types
open Browser
open Shared
