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

[<Test>]
let ``input without attributes`` () =
    formatSourceString false """let i = input []
"""  config
    |> prepend newline
    |> should equal """
let i = input []
"""

[<Test>]
let ``short input with single attribute`` () =
    formatSourceString false """let i = input [ Type "text" ]
"""  config
    |> prepend newline
    |> should equal """
let i = input [ Type "text" ]
"""

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

[<Test>]
let ``div without children or attributes`` () =
    formatSourceString false """let d = div [] []
"""  config
    |> prepend newline
    |> should equal """
let d = div [] []
"""

[<Test>]
let ``div with short attributes`` () =
    formatSourceString false """let d = div [ ClassName "mt-4" ] []
"""  config
    |> prepend newline
    |> should equal """
let d = div [ ClassName "mt-4" ] []
"""


[<Test>]
let ``div with no attributes and short children`` () =
    formatSourceString false """let d = div [] [ str "meh" ]
"""  config
    |> prepend newline
    |> should equal """
let d = div [] [ str "meh" ]
"""

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


[<Test>]
let ``short div with short p`` () =
    formatSourceString false """let d =
    div [] [ p [] [ str "meh" ] ]
"""  config
    |> prepend newline
    |> should equal """
let d = div [] [ p [] [ str "meh" ] ]
"""


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
"""  { config with MaxElmishWidth = 150 }
    |> prepend newline
    |> should equal """
let d =
    div [] [ p [] [ str "fooooooooo" ]; p [] [ str "baaaaaaaar" ] ]
"""

// here the p is 38 characters
// this makes the div multiline but the p not.

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

[<Test>]
let ``div with longer p`` () =
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
        tokenDetailRow
            "FullMatchedLength"
            (span [ ClassName "has-text-weight-semibold" ] [
                ofInt fullMatchedLength
             ])
    ]
]
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
"""  { config with IndentSize = 2 }
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
let ``input with attribute array`` () =
    formatSourceString false """let ia = input [| Type "hidden"; Name "code"; Required "required" |]
"""  config
    |> prepend newline
    |> should equal """
let ia =
    input [| Type "hidden"
             Name "code"
             Required "required" |]
"""

[<Test>]
let ``div with children array`` () =
    formatSourceString false """let d =
    div [||] [| p [||] [| str "oh my foobar" |] |]
"""  config
    |> prepend newline
    |> should equal """
let d =
    div [||] [|
        p [||] [| str "oh my foobar" |]
    |]
"""

[<Test>]
let ``mix lists and array`` () =
    formatSourceString false """let view dispatch model =
    div [| Class "container" |]
        [
          h1 [] [| str "my title" |]
          button [| OnClick (fun _ -> dispatch Msg.Foo) |] [
                str "click me"
          ]
        ]
"""  config
    |> prepend newline
    |> should equal """
let view dispatch model =
    div [| Class "container" |] [
        h1 [] [| str "my title" |]
        button [| OnClick(fun _ -> dispatch Msg.Foo) |] [
            str "click me"
        ]
    ]
"""

[<Test>]
let ``short feliz element`` () =
    formatSourceString false """let a =
    Html.h1 [ prop.text "some title" ]
"""  config
    |> prepend newline
    |> should equal """
let a = Html.h1 [ prop.text "some title" ]
"""

[<Test>]
let ``multiline feliz element`` () =
    formatSourceString false """let a =
        Html.button [
            prop.style [ style.marginLeft 5 ]
            prop.onClick (fun _ -> setCount(count - 1))
            prop.text "Decrement"
        ]
"""  { config with SingleArgumentWebMode = true }
    |> prepend newline
    |> should equal """
let a =
    Html.button [
        prop.style [ style.marginLeft 5 ]
        prop.onClick (fun _ -> setCount (count - 1))
        prop.text "Decrement"
    ]
"""

[<Test>]
let ``nested feliz elements`` () =
    formatSourceString false """let a =
    Html.div [
        Html.h1 [ prop.text "short" ]
        Html.button [
            prop.style [ style.marginRight 5 ]
            prop.onClick (fun _ -> setCount(count + 1))
            prop.text "Increment"
        ]
    ]
"""  { config with SingleArgumentWebMode = true }
    |> prepend newline
    |> should equal """
let a =
    Html.div [
        Html.h1 [ prop.text "short" ]
        Html.button [
            prop.style [ style.marginRight 5 ]
            prop.onClick (fun _ -> setCount (count + 1))
            prop.text "Increment"
        ]
    ]
"""

[<Test>]
let ``feliz counter sample`` () =
    formatSourceString false """module App

open Feliz

let counter = React.functionComponent(fun () ->
    let (count, setCount) = React.useState(0)
    Html.div [
        Html.button [
            prop.style [ style.marginRight 5 ]
            prop.onClick (fun _ -> setCount(count + 1))
            prop.text "Increment"
        ]

        Html.button [
            prop.style [ style.marginLeft 5 ]
            prop.onClick (fun _ -> setCount(count - 1))
            prop.text "Decrement"
        ]

        Html.h1 count
    ])

open Browser.Dom

ReactDOM.render(counter, document.getElementById "root")
"""  { config with SingleArgumentWebMode = true }
    |> prepend newline
    |> should equal """
module App

open Feliz

let counter =
    React.functionComponent (fun () ->
        let (count, setCount) = React.useState (0)
        Html.div [
            Html.button [
                prop.style [ style.marginRight 5 ]
                prop.onClick (fun _ -> setCount (count + 1))
                prop.text "Increment"
            ]

            Html.button [
                prop.style [ style.marginLeft 5 ]
                prop.onClick (fun _ -> setCount (count - 1))
                prop.text "Decrement"
            ]

            Html.h1 count
        ])

open Browser.Dom

ReactDOM.render (counter, document.getElementById "root")
"""

[<Test>]
let ``feliz syntax`` () =
    formatSourceString false """
Html.h1 42

Html.div "Hello there!"

Html.div [ Html.h1 "So lightweight" ]

Html.ul [
  Html.li "One"
  Html.li [ Html.strong "Two" ]
  Html.li [ Html.em "Three" ]
]
"""  { config with SingleArgumentWebMode = true }
    |> prepend newline
    |> should equal """
Html.h1 42

Html.div "Hello there!"

Html.div [ Html.h1 "So lightweight" ]

Html.ul [
    Html.li "One"
    Html.li [ Html.strong "Two" ]
    Html.li [ Html.em "Three" ]
]
"""