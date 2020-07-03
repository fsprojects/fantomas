# Formatting Elmish style guide

This guide explains the main reasoning of how Fantomas formats "Elmish" inspired code.

## Scope

To keep things focused, the scope is currently limited to the Fable.React bindings:
- https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Standard.fs
- https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Props.fs

Variants like Feliz and Fabulous might be covered in the future as well.

## Key concepts

There are two active patterns for [SynExpr](https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-syntaxtree-synexpr.html) that capture the shapes in the Elmish DSL.

See [SourceParser.fs](../src/Fantomas/SourceParser.fs)
 
```fsharp
let (|ElmishReactWithoutChildren|_|) e = ...

let (|ElmishReactWithChildren|_|) e =
````

`ElmishReactWithoutChildren` captures unary tags like `<input />` or `<br />`.
Translated in the F# DSL they match a function that takes a single list as arguments.

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

`ElmishReactWithChildren` captures the non-unary tags like `<p>...</p>` or `<div>...</div>`.
Translated in the F# DSL they match a function that takes two lists as arguments.
The first argument matches the same rules as the unary tag.

While the second argument starts its opening bracket right after the closing of the attributes.
The closing bracket of the children matches the start column of the tag.
Unless the entire expression is short.

Children have one extra indent starting from the parent tag column.


```fsharp
// short
let myParagraph = p [] [ str "short" ]

// long
let myContainer =
    div [ ClassName "container" ] [
        h1 [] [ str "my title" ]
    ]
```

When the children argument is empty, it is place right after the attributes.

```fsharp
let x =
    div [ OnClick (fun _ -> prinftn "meh"
          ClassName "container" ] []
```
