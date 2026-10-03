(*---
fsharp_multiline_bracket_style = stroustrup
---*)
let a =
    View.Entry(
                                    placeholder = "User name",
                                    isEnabled = (not model.IsSigningIn),
                                    textChanged = (fun args -> (dispatch (UserNameChanged args.NewTextValue))))
