module Fantomas.Core.Tests.NamedParameterTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup }

[<Test>]
let ``long named arguments should go on newline`` () =
    formatSourceString
        false
        """let view (model: Model) dispatch =
    View.ContentPage(
        appearing=(fun () -> dispatch PageAppearing),
        title=model.Planet.Info.Name,
        backgroundColor=Color.Black,
        content=["....long line....................................................................................................."]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let view (model: Model) dispatch =
    View.ContentPage(
        appearing = (fun () -> dispatch PageAppearing),
        title = model.Planet.Info.Name,
        backgroundColor = Color.Black,
        content = [
            "....long line....................................................................................................."
        ]
    )
"""

[<Test>]
let ``single view entry`` () =
    formatSourceString
        false
        """
let a =
    View.Entry(
                                    placeholder = "User name",
                                    isEnabled = (not model.IsSigningIn),
                                    textChanged = (fun args -> (dispatch (UserNameChanged args.NewTextValue))))
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    View.Entry(
        placeholder = "User name",
        isEnabled = (not model.IsSigningIn),
        textChanged = (fun args -> (dispatch (UserNameChanged args.NewTextValue)))
    )
"""

[<Test>]
let ``fabulous view`` () =
    formatSourceString
        false
        """
    let loginPage =
        View.ContentPage(
            title = "Fabulous Demo",
            content = View.ScrollView(
                content = View.StackLayout(
                    padding = 30.0,
                    children = [
                        View.Frame(
                            verticalOptions = LayoutOptions.CenterAndExpand,
                            content = View.StackLayout(children = [
                                View.Entry(
                                    placeholder = "User name",
                                    isEnabled = (not model.IsSigningIn),
                                    textChanged = (fun args -> (dispatch (UserNameChanged args.NewTextValue))))
                                View.Entry(
                                    placeholder = "Password",
                                    isPassword = true,
                                    isEnabled = (not model.IsSigningIn),
                                    textChanged = (fun args -> (dispatch (PasswordChanged args.NewTextValue))))
                                View.Button(
                                    text = "Sign in",
                                    heightRequest = 30.0,
                                    isVisible = (not model.IsSigningIn),
                                    command = (fun () -> dispatch SignIn),
                                    canExecute = model.IsCredentialsProvided)
                                View.ActivityIndicator(
                                    isRunning = true,
                                    heightRequest = 30.0,
                                    isVisible = model.IsSigningIn)])
                        )
                    ]
                )
            )
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let loginPage =
    View.ContentPage(
        title = "Fabulous Demo",
        content =
            View.ScrollView(
                content =
                    View.StackLayout(
                        padding = 30.0,
                        children = [
                            View.Frame(
                                verticalOptions = LayoutOptions.CenterAndExpand,
                                content =
                                    View.StackLayout(
                                        children = [
                                            View.Entry(
                                                placeholder = "User name",
                                                isEnabled = (not model.IsSigningIn),
                                                textChanged =
                                                    (fun args -> (dispatch (UserNameChanged args.NewTextValue)))
                                            )
                                            View.Entry(
                                                placeholder = "Password",
                                                isPassword = true,
                                                isEnabled = (not model.IsSigningIn),
                                                textChanged =
                                                    (fun args -> (dispatch (PasswordChanged args.NewTextValue)))
                                            )
                                            View.Button(
                                                text = "Sign in",
                                                heightRequest = 30.0,
                                                isVisible = (not model.IsSigningIn),
                                                command = (fun () -> dispatch SignIn),
                                                canExecute = model.IsCredentialsProvided
                                            )
                                            View.ActivityIndicator(
                                                isRunning = true,
                                                heightRequest = 30.0,
                                                isVisible = model.IsSigningIn
                                            )
                                        ]
                                    )
                            )
                        ]
                    )
            )
    )
"""
