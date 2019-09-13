module Fantomas.Tests.ElmishTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``long named arguments should go on newline`` () =
    formatSourceString false """let view (model: Model) dispatch =
    View.ContentPage(
        appearing=(fun () -> dispatch PageAppearing),
        title=model.Planet.Info.Name,
        backgroundColor=Color.Black,
        content=["....long line....................................................................................................."]
    )
"""  config
    |> prepend newline
    |> should equal """
let view (model: Model) dispatch =
    View.ContentPage
        (appearing = (fun () -> dispatch PageAppearing), title = model.Planet.Info.Name, backgroundColor = Color.Black,
         content =
             [ "....long line....................................................................................................." ])
"""