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
        (appearing = (fun () -> dispatch PageAppearing),
         title = model.Planet.Info.Name,
         backgroundColor = Color.Black,
         content =
             [ "....long line....................................................................................................." ])
"""

[<Test>]
let ``single view entry`` () =
    formatSourceString false """
let a =
    View.Entry(
                                    placeholder = "User name",
                                    isEnabled = (not model.IsSigningIn),
                                    textChanged = (fun args -> (dispatch (UserNameChanged args.NewTextValue))))
"""  config
    |> prepend newline
    |> should equal """
let a =
    View.Entry
        (placeholder = "User name",
         isEnabled = (not model.IsSigningIn),
         textChanged = (fun args -> (dispatch (UserNameChanged args.NewTextValue))))
"""

[<Test>]
[<Ignore("tests works but takes way too long")>]
let ``fabulous view`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
let loginPage =
    View.ContentPage
        (title = "Fabulous Demo",
         content =
             View.ScrollView
                 (content =
                     View.StackLayout
                         (padding = 30.0,
                          children =
                              [ View.Frame
                                  (verticalOptions = LayoutOptions.CenterAndExpand,
                                   content =
                                       View.StackLayout
                                           (children =
                                               [ View.Entry
                                                   (placeholder = "User name",
                                                    isEnabled = (not model.IsSigningIn),
                                                    textChanged =
                                                        (fun args -> (dispatch (UserNameChanged args.NewTextValue))))
                                                 View.Entry
                                                     (placeholder = "Password",
                                                      isPassword = true,
                                                      isEnabled = (not model.IsSigningIn),
                                                      textChanged =
                                                          (fun args -> (dispatch (PasswordChanged args.NewTextValue))))
                                                 View.Button
                                                     (text = "Sign in",
                                                      heightRequest = 30.0,
                                                      isVisible = (not model.IsSigningIn),
                                                      command = (fun () -> dispatch SignIn),
                                                      canExecute = model.IsCredentialsProvided)
                                                 View.ActivityIndicator
                                                     (isRunning = true,
                                                      heightRequest = 30.0,
                                                      isVisible = model.IsSigningIn) ])) ])))
"""

(*
Formatting Elmish Code ~ An attempt by Florian Verdonck

So people have been asking for alternative way of formatting Elmish code.
At best, they dictate that is should be better. While agreeing it should, the main question remains: how?

In the following unit tests, I tried to come up with something that makes sense.
This is by all means not the final outcome, this merely serves as a starting point.
And I hope it triggers the correct conversation about the topic.

To keep things focused, I limited the scope to the Fable.React bindings:
- https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Standard.fs
- https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Props.fs

I would also like to tackle variants like Feliz and Fabulous, however, all in good time.
Initially, I want to tackle Fable.React style and see where it goes.

*)

(*

In SourceParser, I created two active patterns to capture html element with and without children.

With:

identifier [list of attributes] [list of children]

Without:

identifier [list of attributes]

These are then printed out in genExpr of CodePrinter.
In the following unit tests I will try to explain the reasoning behind certain decisions.
Again nothing is set in stone, this is just an initial attempt.

Some general rules:

- If things are short (short being determined by a setting) put them on one line
- If things are multiline, align attributes and children so they are easy to distinguish.
- Multiline attributes are formatted:
    [ a1
      a2
      a3 ]
  While multiline children are formatted:
  identifier attributes [ c1
      c2
      c3
  ]

  the closing ] aligns with the identifier, children have one indent.

*)

// everything is short, so single line

[<Test>]
let ``input without attributes`` () =
    formatSourceString false """let i = input []
"""  config
    |> prepend newline
    |> should equal """
let i = input []
"""

// everything is short, so single line

[<Test>]
let ``short input with single attribute`` () =
    formatSourceString false """let i = input [ Type "text" ]
"""  config
    |> prepend newline
    |> should equal """
let i = input [ Type "text" ]
"""

// the attributes are multiline, they follow the natural way of how multiline arrays are formatted by default in Fantomas
// one difference there is that will always but place next to the identifier (input, in this case)

[<Test>]
let ``multiline input with multiple attributes`` () =
    formatSourceString false """let i = input [ Type "text"; Required "required" ]
"""  config
    |> prepend newline
    |> should equal """
let i =
    input [ Type "text"
            Required "required" ]
"""

// everything is short, so single line

[<Test>]
let ``div without children or attributes`` () =
    formatSourceString false """let d = div [] []
"""  config
    |> prepend newline
    |> should equal """
let d = div [] []
"""

// everything is short, so single line

[<Test>]
let ``div with short attributes`` () =
    formatSourceString false """let d = div [ ClassName "mt-4" ] []
"""  config
    |> prepend newline
    |> should equal """
let d = div [ ClassName "mt-4" ] []
"""

// here, it is immediately visible that the div has no children
// the empty list is placed next to the closing ] of the attributes

[<Test>]
let ``div with multiline attributes`` () =
    formatSourceString false """let d = div [ ClassName "container"; OnClick (fun _ -> printfn "meh")  ] []
"""  config
    |> prepend newline
    |> should equal """
let d =
    div [ ClassName "container"
          OnClick(fun _ -> printfn "meh") ] []
"""

// everything is short, so single line

[<Test>]
let ``div with no attributes and short no-elmish children`` () =
    formatSourceString false """let d = div [] [ str "meh" ]
"""  config
    |> prepend newline
    |> should equal """
let d = div [] [ str "meh" ]
"""

// multiple children
// one indent further than the start of the parent identifier

[<Test>]
let ``div with not attributes and multiple elmish children`` () =
    formatSourceString false """let d =
    div [] [
      span [] [ str "a" ]
      span [] [ str "b" ]
    ]
"""  config
    |> prepend newline
    |> should equal """
let d =
    div [] [
        span [] [ str "a" ]
        span [] [ str "b" ]
    ]
"""

// same example as above but with short attributes

[<Test>]
let ``div with single attribute and children`` () =
    formatSourceString false """let view =
    div [ ClassName "container" ] [
        h1 [] [ str "A heading 1" ]
        p [] [ str "A paragraph" ]
    ]
"""  config
    |> prepend newline
    |> should equal """
let view =
    div [ ClassName "container" ] [
        h1 [] [ str "A heading 1" ]
        p [] [ str "A paragraph" ]
    ]
"""

// long attributes and long children

[<Test>]
let ``div with multiple attributes and children`` () =
    formatSourceString false """let d =
div [ ClassName "container"; OnClick (fun _ -> printfn "meh") ] [
    span [] [str "foo"]
    code [] [str "bar"]
]
"""  config
    |> prepend newline
    |> should equal """
let d =
    div [ ClassName "container"
          OnClick(fun _ -> printfn "meh") ] [
        span [] [ str "foo" ]
        code [] [ str "bar" ]
    ]
"""

// if there is a single child that is short, keep it on one line

[<Test>]
let ``short div with short p`` () =
    formatSourceString false """let d =
    div [] [ p [] [ str "meh" ] ]
"""  config
    |> prepend newline
    |> should equal """
let d = div [] [ p [] [ str "meh" ] ]
"""

// again, as long as thing are short, keep them in one line

[<Test>]
let ``short div with multiple short children`` () =
    formatSourceString false """let d =
    div [] [
      br [] ; br []
    ]
"""  config
    |> prepend newline
    |> should equal """
let d = div [] [ br []; br [] ]
"""

[<Test>]
let ``div with long children but a long setting`` () =
    formatSourceString false """let d =
    div [] [
        p [] [ str "fooooooooo" ]
        p [] [ str "baaaaaaaar" ]
    ]
"""  { config with MaxArrayOrListWidth = 150 }
    |> prepend newline
    |> should equal """
let d =
    div [] [ p [] [ str "fooooooooo" ]; p [] [ str "baaaaaaaar" ] ]
"""

// here the p is 38 characters
// this makes the div multiline
// but here the is only one child that is short, so the closing ] of list says on the same line

[<Test>]
let ``short div with slightly longer p`` () =
    formatSourceString false """let d =
    div [] [ p [] [ str "meeeeeeeeeeeeeeeeeeeeeh" ] ]
"""  config
    |> prepend newline
    |> should equal """
let d =
    div [] [
        p [] [ str "meeeeeeeeeeeeeeeeeeeeeh" ]
    ]
"""

// here is it is easier to spot there the div ends and where the p ends because of the extra indent.

[<Test>]
let ``short div with longer p`` () =
    formatSourceString false """let d =
    div [] [ p [] [ str "meeeeeeeeeeeeeeeeeeeeehhhh" ] ]
"""  config
    |> prepend newline
    |> should equal """
let d =
    div [] [
        p [] [
            str "meeeeeeeeeeeeeeeeeeeeehhhh"
        ]
    ]
"""

[<Test>]
let counter () =
    formatSourceString false """
let view model dispatch =
  div [] [
    button [ OnClick (fun _ -> dispatch Decrement) ] [
        str "-"
    ]
    div [] [
        str (sprintf "%A" model)
    ]
    button [ OnClick (fun _ -> dispatch Increment) ] [
        str "+"
    ]
  ]
"""  config
    |> prepend newline
    |> should equal """
let view model dispatch =
    div [] [
        button [ OnClick(fun _ -> dispatch Decrement) ] [
            str "-"
        ]
        div [] [ str (sprintf "%A" model) ]
        button [ OnClick(fun _ -> dispatch Increment) ] [
            str "+"
        ]
    ]
"""

[<Test>]
let ``view entry`` () =
    formatSourceString false """
let viewEntry todo dispatch =
  li [ classList [ ("completed", todo.completed); ("editing", todo.editing) ] ]
     [ div [ ClassName "view" ]
           [ input [ ClassName "toggle"
                     Type "checkbox"
                     Checked todo.completed
                     OnChange (fun _ -> Check (todo.id,(not todo.completed)) |> dispatch) ]
             label [ OnDoubleClick (fun _ -> EditingEntry (todo.id,true) |> dispatch) ]
                   [ str todo.description ]
             button [ ClassName "destroy"
                      OnClick (fun _-> Delete todo.id |> dispatch) ]
                    []
           ]
       input [ ClassName "edit"
               valueOrDefault todo.description
               Name "title"
               Id ("todo-" + (string todo.id))
               OnInput (fun ev -> UpdateEntry (todo.id, !!ev.target?value) |> dispatch)
               OnBlur (fun _ -> EditingEntry (todo.id,false) |> dispatch)
               onEnter (EditingEntry (todo.id,false)) dispatch ]
    ]
"""  config
    |> prepend newline
    |> should equal """
let viewEntry todo dispatch =
    li [ classList [ ("completed", todo.completed)
                     ("editing", todo.editing) ] ] [
        div [ ClassName "view" ] [
            input [ ClassName "toggle"
                    Type "checkbox"
                    Checked todo.completed
                    OnChange(fun _ -> Check(todo.id, (not todo.completed)) |> dispatch) ]
            label [ OnDoubleClick(fun _ -> EditingEntry(todo.id, true) |> dispatch) ] [
                str todo.description
            ]
            button [ ClassName "destroy"
                     OnClick(fun _ -> Delete todo.id |> dispatch) ] []
        ]
        input [ ClassName "edit"
                valueOrDefault todo.description
                Name "title"
                Id("todo-" + (string todo.id))
                OnInput(fun ev ->
                    UpdateEntry(todo.id, !!ev.target?value)
                    |> dispatch)
                OnBlur(fun _ -> EditingEntry(todo.id, false) |> dispatch)
                onEnter (EditingEntry(todo.id, false)) dispatch ]
    ]
"""

[<Test>]
let ``multiline attributes, no children`` () =
    formatSourceString false """let a =
               button [ ClassName "destroy"
                        OnClick(fun _-> Delete todo.id |> dispatch) ]
                      []
"""  config
    |> prepend newline
    |> should equal """
let a =
    button [ ClassName "destroy"
             OnClick(fun _ -> Delete todo.id |> dispatch) ] []
"""

[<Test>]
let ``table and tbody`` () =
    formatSourceString false """
table [ ClassName "table table-striped table-hover mb-0" ]
              [ tbody []
                    [ tokenDetailRow "TokenName" (str tokenName)
                      tokenDetailRow "LeftColumn" (ofInt leftColumn)
                      tokenDetailRow "RightColumn" (ofInt rightColumn)
                      tokenDetailRow "Content" (pre [] [ code [] [ str token.Content ] ])
                      tokenDetailRow "ColorClass" (str colorClass)
                      tokenDetailRow "CharClass" (str charClass)
                      tokenDetailRow "Tag" (ofInt tag)
                      tokenDetailRow "FullMatchedLength"
                          (span [ ClassName "has-text-weight-semibold" ] [ ofInt fullMatchedLength ]) ] ]
"""  config
    |> prepend newline
    |> should equal """
table [ ClassName "table table-striped table-hover mb-0" ] [
    tbody [] [
        tokenDetailRow "TokenName" (str tokenName)
        tokenDetailRow "LeftColumn" (ofInt leftColumn)
        tokenDetailRow "RightColumn" (ofInt rightColumn)
        tokenDetailRow "Content" (pre [] [ code [] [ str token.Content ] ])
        tokenDetailRow "ColorClass" (str colorClass)
        tokenDetailRow "CharClass" (str charClass)
        tokenDetailRow "Tag" (ofInt tag)
        tokenDetailRow "FullMatchedLength"
            (span [ ClassName "has-text-weight-semibold" ] [
                ofInt fullMatchedLength
             ])
    ]
]
"""

[<Test>]
let ``div with single child that is short`` () =
    formatSourceString false """
div [] [ p [] [ str "meh" ] ]
"""  { config with MaxArrayOrListWidth = 5 }
    |> prepend newline
    |> should equal """
div [] [
    p [] [
        str "meh"
    ]
]
"""

[<Test>]
let ``short expression without attributes`` () =
    formatSourceString false """p [] [ str "meh" ]
"""  config
    |> prepend newline
    |> should equal """
p [] [ str "meh" ]
"""

[<Test>]
let ``child with empty children`` () =
    formatSourceString false """
let commands dispatch =
    Button.button
        [ Button.Color Primary
          Button.Custom
              [ ClassName "rounded-0"
                OnClick(fun _ -> dispatch GetTrivia) ] ]
        [ i [ ClassName "fas fa-code mr-1" ] []
          str "Get trivia" ]
"""  config
    |> prepend newline
    |> should equal """
let commands dispatch =
    Button.button [ Button.Color Primary
                    Button.Custom [ ClassName "rounded-0"
                                    OnClick(fun _ -> dispatch GetTrivia) ] ] [
        i [ ClassName "fas fa-code mr-1" ] []
        str "Get trivia"
    ]
"""

[<Test>]
let ``clock with two spaces`` () =
    formatSourceString false """
let view (CurrentTime time) dispatch =
    svg
      [ ViewBox "0 0 100 100"
        SVG.Width "350px" ]
      [ circle
          [ Cx "50"
            Cy "50"
            R "45"
            SVG.Fill "#0B79CE" ] []
        // Hours
        clockHand (Hour time.Hour) "lightgreen" "2" 25.0
        handTop time.Hour "lightgreen" 25.0 12.0
        // Minutes
        clockHand (Minute time.Minute) "white" "2" 35.0
        handTop time.Minute "white" 35.0 60.0
        // Seconds
        clockHand (Second time.Second) "#023963" "1" 40.0
        handTop time.Second "#023963" 40.0 60.0
        // circle in the center
        circle
          [ Cx "50"
            Cy "50"
            R "3"
            SVG.Fill "#0B79CE"
            SVG.Stroke "#023963"
            SVG.StrokeWidth 1.0 ] []
      ]
"""  { config with IndentSpaceNum = 2 }
    |> prepend newline
    |> should equal """
let view (CurrentTime time) dispatch =
  svg [ ViewBox "0 0 100 100"
        SVG.Width "350px" ] [
    circle [ Cx "50"
             Cy "50"
             R "45"
             SVG.Fill "#0B79CE" ] []
    // Hours
    clockHand (Hour time.Hour) "lightgreen" "2" 25.0
    handTop time.Hour "lightgreen" 25.0 12.0
    // Minutes
    clockHand (Minute time.Minute) "white" "2" 35.0
    handTop time.Minute "white" 35.0 60.0
    // Seconds
    clockHand (Second time.Second) "#023963" "1" 40.0
    handTop time.Second "#023963" 40.0 60.0
    // circle in the center
    circle [ Cx "50"
             Cy "50"
             R "3"
             SVG.Fill "#0B79CE"
             SVG.Stroke "#023963"
             SVG.StrokeWidth 1.0 ] []
  ]
"""

[<Test>]
let ``mehh`` () =
    formatSourceString false """let x =
Opts.oneOf
                                            (Optional,
                                             [ Opt.flag [ "third"; "f" ]
                                               Opt.valueWith "new value" [ "fourth"; "ssssssssssssssssssssssssssssssssssssssssssssssssssss" ] ])
"""  config
    |> prepend newline
    |> should equal """
let x =
    Opts.oneOf
        (Optional,
         [ Opt.flag [ "third"; "f" ]
           Opt.valueWith "new value"
               [ "fourth"
                 "ssssssssssssssssssssssssssssssssssssssssssssssssssss" ] ])
"""