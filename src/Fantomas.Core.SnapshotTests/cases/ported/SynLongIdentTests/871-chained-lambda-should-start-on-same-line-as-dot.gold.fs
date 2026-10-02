namespace LoginWithBulmaTutorial

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Notation
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type MySPA = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<SPAEntryPoint>]
    let Main () =
        let passwordValid = Var.Create true
        let emailValid = Var.Create true

        MySPA()
            .AttrPassword(Attr.ClassPred "is-danger" (not passwordValid.V))
            .AttrEmail(Attr.ClassPred "is-danger" (not emailValid.V))
            .Login(fun e ->
                passwordValid
                := not (String.IsNullOrWhiteSpace e.Vars.Password.Value)

                emailValid
                := not (String.IsNullOrWhiteSpace e.Vars.Email.Value)

                if passwordValid.Value && emailValid.Value then
                    JS.Alert(sprintf "Your email is %s" e.Vars.Email.Value)

                e.Event.PreventDefault())
            .Bind()
