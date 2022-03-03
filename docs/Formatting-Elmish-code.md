# Formatting Elmish style guide

This guide explains the main reasoning of how Fantomas formats "Elmish" inspired code.

## Scope

To keep things focused, the scope is currently limited to the Fable.React and Feliz bindings:
- https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Standard.fs
- https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Props.fs
- https://github.com/Zaid-Ajaj/Feliz/blob/master/Feliz/Html.fs
- https://github.com/Zaid-Ajaj/Feliz/blob/master/Feliz/Properties.fs

[Fabulous](https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/view.html) might be covered in the future as well.

## Key concepts

There are two active patterns for [SynExpr](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html) that capture the shapes in the Elmish DSL.

See [SourceParser](../src/Fantomas/SourceParser.fs#:~:text=elmishreactwithoutchildren):
 
```fsharp
let (|ElmishReactWithoutChildren|_|) e = ...

let (|ElmishReactWithChildren|_|) e = ...
````

### ElmishReactWithoutChildren

Captures unary tags like `<input />` or `<br />`.
Translated in the F# DSL, they match a function that takes a single list as arguments.

```fsharp
let i = input [ Type "hidden" ]
```

The props or attributes parameter is formatted like a normal list or array would be in default Fantomas.

```fsharp
// short
myTag [ a1; a2 ]

// long
myTag [ a1
        a2
        a3 ]
```

The tag and attributes will always align.

#### Feliz

Translated in the [Feliz DSL](https://zaid-ajaj.github.io/Feliz/#/Feliz/Syntax), tags with children also take a single list as arguments.

When [fsharp_single_argument_web_mode](./Documentation.md#fsharp_single_argument_web_mode) is `true`, props and children have one extra indent starting from the parent tag column.
The opening bracket starts right after the tag, and the closing bracket matches the start column of the tag.

```fsharp
let myContainer =
    Html.div [
        prop.className "container"
        prop.children [
            Html.h1 "my title"
        ]
    ]
```

### ElmishReactWithChildren

Captures the non-unary tags like `<p>...</p>` or `<div>...</div>`.
Translated in the F# DSL, they match a function that takes two lists as arguments.
The first argument matches the same rules as the unary tag.

The second argument starts its opening bracket right after the closing of the attributes.
The closing bracket of the children matches the start column of the tag unless the entire expression is short.

Like with `fsharp_single_argument_web_mode`, children have one extra indent starting from the parent tag column.


```fsharp
// short
let myParagraph = p [] [ str "short" ]

// long
let myContainer =
    div [ ClassName "container" ] [
        h1 [] [ str "my title" ]
    ]
```

When the children argument is empty, it is placed right after the attributes.

```fsharp
let x =
    div [ OnClick(fun _ -> printfn "meh")
          ClassName "container" ] []
```
