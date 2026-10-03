open System

type InputKind =
    | Text of placeholder: string option
    | Password of placeholder: string option

type InputOptions =
    { Label: string option
      Kind: InputKind
      Validators: (string -> bool) array }

type InputBuilder() =
    member t.Yield(_) =
        { Label = None
          Kind = Text None
          Validators = [||] }

    [<CustomOperation("text")>]
    member this.Text(io, ?placeholder) = { io with Kind = Text placeholder }

    [<CustomOperation("password")>]
    member this.Password(io, ?placeholder) = { io with Kind = Password placeholder }

    [<CustomOperation("label")>]
    member this.Label(io, label) = { io with Label = Some label }

    [<CustomOperation("with_validators")>]
    member this.Validators(io, [<ParamArray>] validators) = { io with Validators = validators }

let input = InputBuilder()

let name =
    input {
        label "Name"
        text
        with_validators (String.IsNullOrWhiteSpace >> not)
    }

let email =
    input {
        label "Email"
        text "Your email"
        with_validators (String.IsNullOrWhiteSpace >> not) (fun s -> s.Contains "@")
    }

let password =
    input {
        label "Password"
        password "Must contains at least 6 characters, one number and one uppercase"
        with_validators (String.exists Char.IsUpper) (String.exists Char.IsDigit) (fun s -> s.Length >= 6)
    }
