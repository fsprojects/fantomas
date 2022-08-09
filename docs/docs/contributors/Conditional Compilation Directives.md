---
category: Contributors
categoryindex: 2
index: 8
---
# Formatting Conditional Compilation Directives

Fantomas is able to format code that contains [conditional compiler directives](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-directives#conditional-compilation-directives).  
In order to achieve this, Fantomas will actually format the code multiple times and merge all results afterwards.

## Compilation directives and the syntax tree

The F# parser will construct a different syntax tree based on the provided compilation directives.  
Consider the following piece of code:

```fsharp
let a =
    #if DEBUG
    0
    #else
    1
    #endif
```

When parsing this code without any directives, the `#else` branch will be considered the active code path.  
The AST would be:

```fsharp
ImplFile
  (ParsedImplFileInput
     ("tmp.fsx", true, QualifiedNameOfFile Tmp$fsx, [], [],
      [SynModuleOrNamespace
         ([Tmp], false, AnonModule,
          [Let
             (false,
              [SynBinding
                 (None, Normal, false, false, [],
                  PreXmlDoc ((1,0), FSharp.Compiler.Xml.XmlDocCollector),
                  SynValData
                    (None, SynValInfo ([], SynArgInfo ([], false, None)), None),
                  Named (SynIdent (a, None), false, None, tmp.fsx (1,4--1,5)),
                  None, Const (Int32 1, tmp.fsx (5,4--5,5)), tmp.fsx (1,4--1,5),
                  Yes tmp.fsx (1,0--5,5),
                  { LetKeyword = Some tmp.fsx (1,0--1,3)
                    EqualsRange = Some tmp.fsx (1,6--1,7) })],
              tmp.fsx (1,0--5,5))], PreXmlDocEmpty, [], None,
          tmp.fsx (1,0--6,10), { ModuleKeyword = None
                                 NamespaceKeyword = None })], (false, false),
      { ConditionalDirectives =
         [If (Ident "DEBUG", tmp.fsx (2,4--2,13)); Else tmp.fsx (4,4--4,9);
          EndIf tmp.fsx (6,4--6,10)]
        CodeComments = [] }))
```

Notice that the right hand expression of binding `a` is `Const (Int32 1, ...)`.
There is no mention of `0` as that code was not active and thus is not a part of the syntax tree.

Passing `[ "DEBUG" ]` to the parser will influence the lexer. The lexer will tokenize the other code branch and take the `#if DEBUG` path this time.  
Leading to

```fsharp
ImplFile
  (ParsedImplFileInput
     ("tmp.fsx", true, QualifiedNameOfFile Tmp$fsx, [], [],
      [SynModuleOrNamespace
         ([Tmp], false, AnonModule,
          [Let
             (false,
              [SynBinding
                 (None, Normal, false, false, [],
                  PreXmlDoc ((1,0), FSharp.Compiler.Xml.XmlDocCollector),
                  SynValData
                    (None, SynValInfo ([], SynArgInfo ([], false, None)), None),
                  Named (SynIdent (a, None), false, None, tmp.fsx (1,4--1,5)),
                  None, Const (Int32 0, tmp.fsx (3,4--3,5)), tmp.fsx (1,4--1,5),
                  Yes tmp.fsx (1,0--3,5),
                  { LetKeyword = Some tmp.fsx (1,0--1,3)
                    EqualsRange = Some tmp.fsx (1,6--1,7) })],
              tmp.fsx (1,0--3,5))], PreXmlDocEmpty, [], None,
          tmp.fsx (1,0--6,10), { ModuleKeyword = None
                                 NamespaceKeyword = None })], (false, false),
      { ConditionalDirectives =
         [If (Ident "DEBUG", tmp.fsx (2,4--2,13)); Else tmp.fsx (4,4--4,9);
          EndIf tmp.fsx (6,4--6,10)]
        CodeComments = [] }))
```

This tree is almost identical but the constant value is now `Const (Int32 0, ...)`.

## Multiple trees

As the combination of directives has an influence on the tree, Fantomas first parses the tree without any directives.
This base tree is then being inspected for [ConditionalDirectiveTrivia](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntaxtrivia-conditionaldirectivetrivia.html).  
We determine the different combinations in the `Defines` module.

<div class="mermaid text-center">
graph TD
    A["Parse base tree"] --> B
    B["Figure out all compiler define combinations"] --> C
    B --> D
    C["Format tree []"]
    D["Format tree ['DEBUG']"]
    C --> E
    D --> E
    E["Merge results"]
 </div>

As trivia is being restored in each tree, they all will have gaps in them.

The first result will look like:

```fsharp
let a =
    #if DEBUG
    #else
    1
    #endif
```

and the second:

```fsharp
let a =
    #if DEBUG
    0
    #else
    #endif
```

## Merging the trees

Once every tree is formatted, we chop each file into fragments.
A fragment is everything between a conditional directive `#if | #else | #endif` or an actual directive.  
Each result should have the same amount of fragments before we can merge them together.
If this is not the case, it means that somewhere a trivia was not properly restored.

If the number of fragments add up in each tree, then we merge two trees by reducing both lists and comparing each fragment.  
We always take the longest fragment and thus picking the active code.

```fsharp
// fragments of []
[ "let a ="; "#if DEBUG"; ""; "#else"; "1"; "#endif" ]

// fragments of [ "DEBUG" ]
[ "let a ="; "#if DEBUG"; "0"; "#else"; ""; "#endif" ]
```

After merging:
```fsharp
[ "let a ="; "#if DEBUG"; "0"; "#else"; "1"; "#endif" ]
```

<div class="d-flex justify-content-between my-4">
  <a href="./Formatted%20Code.html">Previous</a>
</div>