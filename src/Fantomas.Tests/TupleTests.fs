module Fantomas.Tests.TupleTests

open NUnit.Framework
open FsUnit

open Fantomas.Tests.TestHelper

[<Test>]
let ``tuple with lamba should add parenthesis`` () =
    formatSourceString false """
let private carouselSample =
    FunctionComponent.Of<obj>(fun _ ->
        fragment [] []
    ,"CarouselSample")
"""  config
    |> should equal """let private carouselSample = FunctionComponent.Of<obj>((fun _ -> fragment [] []), "CarouselSample")
"""

[<Test>]
let ``multiline lamba atCurrentColumn`` () =
    formatSourceString false """let App =
    FunctionComponent.Of
        (fun _ ->
            useRedirect "/components" "/components/alerts" None (Some false)
            let routeResults = useRoutes routes
            let path = usePath()

            match routeResults with
            | Some r -> suspense [ Fallback Loading ] [ layout path r ]
            | None -> NotFoundPage
         ,"App")

let App =
    FunctionComponent.Of
        ((fun _ ->
            useRedirect "/components" "/components/alerts" None (Some false)
            let routeResults = useRoutes routes
            let path = usePath()

            match routeResults with
            | Some r -> suspense [ Fallback Loading ] [ layout path r ]
            | None -> NotFoundPage)
         , "App")"""  config
    |> should equal """let App =
    FunctionComponent.Of
        ((fun _ ->
            useRedirect "/components" "/components/alerts" None (Some false)
            let routeResults = useRoutes routes
            let path = usePath()

            match routeResults with
            | Some r -> suspense [ Fallback Loading ] [ layout path r ]
            | None -> NotFoundPage), "App")

let App =
    FunctionComponent.Of
        ((fun _ ->
            useRedirect "/components" "/components/alerts" None (Some false)
            let routeResults = useRoutes routes
            let path = usePath()

            match routeResults with
            | Some r -> suspense [ Fallback Loading ] [ layout path r ]
            | None -> NotFoundPage), "App")
"""