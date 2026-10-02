(*---
fsharp_experimental_elmish = true
---*)
module CapitalGuardian.App

open Fable.Core.JsInterop
open Fable.React
open Feliz

[<ReactComponent()>]
let private App () =
    div [] [
        str "meh 2000k"
        (*
                          {small && <Navigation />}
              <Container>
                {!small && <Header />}
                {!small && <Navigation />}
                {routeResult || <NotFoundPage />}
              </Container>
              <ToastContainer />
        *)
    ]

exportDefault App
