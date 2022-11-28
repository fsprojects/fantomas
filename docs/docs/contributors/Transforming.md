---
category: Contributors
categoryindex: 2
index: 5
---
# Fantomas.Core overview (1)

In its simplest form, Fantomas.Core works in two major phases: transform the raw source code to a custom tree model, traverse the tree and print the formatted code.

<div class="mermaid text-center">
graph TD
    A[Transform source code to Oak] --> B
    B[Traverse Oak to get formatted code] --> C[Formatted code]
    style A stroke:#338CBB,stroke-width:2px
 </div>

Unfortunately, both phases are not always very straight forward.  
But once you get hang of the first phase, you can easily understand the second phase.

## Processing the raw source

To have an understanding of what the raw source code means we parse the code using the parser of the F# compiler.  
We can parse the source code into an untyped syntax tree. This tree isn't really perfect for our use-case, so we map it to an `Oak`.  
An `Oak` is the toplevel root node of the Fantomas tree. We use a custom tree because it better suites our needs to reconstruct the output code.


<div class="mermaid text-center">
graph TD
    A[Parse AST] --> B
    B[Transfrom untyped AST to OAK] --> C
    C[Enrich the Oak with Trivia]
</div>

### Parse AST

Parsing the AST is straightforward. We use the `parseFile` function from `Fantomas.FCS` and get back a syntax tree and diagnostic information.  
The diagnostic information is used to report errors and warnings. When we have errors, we stop processing the file.

**Fantomas requires a valid source code to format.**

If your code has errors, the parser cannot return a complete AST which is a strict requirement to run the remaining phases.

```fsharp
let a =
```

Returns a parsing error `Incomplete structured construct at or before this point in binding`.
This has `FSharpDiagnosticSeverity.Error` and Fantomas will not process any code that has those.

When there are only warnings, Fantomas will still try to process the file but will always yield a result.

`parseFile` takes three parameters:
- `isSignature: bool`

The Syntax tree for a signature is a little different than a regular source file. The parser needs to know this and throughout the rest of the process, signature ASTs are treated differently.

- `sourceText: ISourceText`

The input source code string is converted to an [ISourceText](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-text-isourcetext.html)) internally.

- `defines: string list`

Conditional compilation defines are passed to the parser. These can have an influence on the parsing process, resulting in different ASTs.

```fsharp
let a =
    #if DEBUG
    0
    #else
    1
    #endif
```

Depending on the defines `[]` or `["DEBUG"]`, the AST will be different.
The tree will also be created based on a single code path.

### Transform untyped AST to OAK

The untyped syntax tree from the F# compiler is used as an intermediate representation of source code in the process of transforming a text file to binary.  
The AST has is optimized for the use-case of moving the plot forward towards binary. What we try to do in Fantomas is stop at the AST level and go back to source text.

The F# compiler was never designed with our use-case in mind and yet it has served us very well for years. 
In the past we did not have our own tree and were able to pull of formatting by traversing the compiler tree.
This of course had its limitations and we had to overcome these with some hacks.

Alas, some things in the AST aren't shaped the way we would like them to be. Sometimes, there is too much information, other times to little.
To stream line our entire process, we've decide to map the untyped tree to our own custom object model.
This introduces a lot of flexibility and simplifies our story.

> Fangorn, what drove them into that madness - Gimli

In `Fangorn.fs` we map the AST to our tree model. Some of the benefit we get out of this:
- The Oak model does not differentiate between implementation files and signature files. We use one tree model which allow for optimal code re-use in `CodePrinter2.fs`.
- We don't map all possible combinations of AST into our model. Sometimes valid AST code can in theory be created, 
  but will in practise never exist. For example `expection Repr` in `SynTypeDefn`. It is part of the defined in `SyntaxTree.fs` yet the parser will never create it.
  The F# compiler uses this later in the typed tree. We will throw an exception when encountering this during the mapping as we have the foresight of what the parser doesn't create.
- Recursive types are all consider as toplevel types. This is not the case in the AST but we map it as such.
- Some nodes are combined into one, for example a toplevel attribute will always be linked to its sibling do expression.
- The ranges of some nodes are being calculated when they lead to a more accurate result.

### Collect Trivia

A syntax tree contains almost all the information we need to format the code. 
However, there are three items that are either missing all together or require further processing:

- Blank lines
- Code comments
- Conditional directives

These three items are labelled as `Trivia` in Fantomas. We need to restore them because the source code originally had them, but cannot do so purely on the AST.

#### Detecting trivia

Trivia can however be easily detected in Fantomas. Both code comments and conditional directives are present in the AST.
These are stored on the file level (in [ParsedImplFileInput](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-parsedimplfileinput.html) and  [ParsedSigFileInput](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-parsedsigfileinput.html) in the `trivia` node).

In both [ParsedImplFileInputTrivia](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntaxtrivia-parsedimplfileinputtrivia.html) and [ParsedSigFileInputTrivia](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntaxtrivia-parsedsigfileinputtrivia.html) we can see comments and conditional directives.

```fsharp
let a = 
   // comment b
   c
```

roughly translates to

```fsharp
ImplFile
  (ParsedImplFileInput
     ("tmp.fsx", true, QualifiedNameOfFile Tmp$fsx, [], [],
      [SynModuleOrNamespace
         ([Tmp], false, AnonModule,
          [Let
             (false,
              [SynBinding(...)],
              tmp.fsx (1,0--3,4))], PreXmlDocEmpty, [], None, tmp.fsx (1,0--3,4),
          { ModuleKeyword = None
            NamespaceKeyword = None })], (false, false),
      { ConditionalDirectives = []
        CodeComments = [LineComment tmp.fsx (2,3--2,15)] }))
```

The AST does contain a node for the line comment, but we cannot restore it when we are processing the let binding.  
There is no link between the line comment and the let binding.

All trivia face this problem, so we need to process them separately.  
We do this in `Flowering.collectTrivia`.

Note: blank lines are detected differently, we go over all the lines via the `ISourceText`.

#### Inserting trivia

Once we have the trivia, we can insert them to an `Node` they belong to.
This is one of the key reasons why we work with our own tree. We can add the trivia information to the best suitable child node in the `Oak`.
Every `Node` can have `ContentBefore` and `ContentAfter`, this is how we try to reconstruct everything.

<div class="mermaid text-center">
graph TD
    A[Capture all trivia from AST and ISourceText] --> B
    D[Insert trivia into nodes]
 </div>

<fantomas-nav previous="./Solution%20Structure.html" next="./Traverse.html"></fantomas-nav>
