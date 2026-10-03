(*---
fsharp_multiline_bracket_style = stroustrup
---*)
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
