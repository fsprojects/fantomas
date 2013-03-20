module Fantomas.Tests.CodeFormatterExtExtExtTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``type providers``() =
    formatSourceString """
type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">""" config
    |> prepend newline
    |> should equal """
type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">
"""

[<Test>]
let ``named arguments``() =
    formatSourceString """
type SpeedingTicket() =
    member this.GetMPHOver(speed: int, limit: int) = speed - limit

let CalculateFine (ticket : SpeedingTicket) =
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0 else 100.0""" config
    |> prepend newline
    |> should equal """
type SpeedingTicket() = 
    member this.GetMPHOver(speed : int, limit : int) = speed - limit

let CalculateFine(ticket : SpeedingTicket) = 
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0
    else 100.0
"""