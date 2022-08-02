---
category: Contributors
categoryindex: 2
index: 5
---
# Fantomas.Core overview (1)

In its simplest form, Fantomas.Core works in two major phases: prepare the context and process said context.

<div class="mermaid text-center">
graph TD
    A[Prepare Context] --> B
    B[Print AST with Context] --> C[Formatted code]
    style A stroke:#338CBB,stroke-width:2px
 </div>

Unfortunately, both phases are not always very straight forward.  
But once you get hang of the first phase, you can easily understand the second phase.

## Preparing the context

When we need to format a source code file, we need to prepare the context based on the content of the input file and the configuration of the formatter.
The most important aspect of this phase is transforming the source code to a syntax tree. 
However we need to do further processing of the AST before we can simply print it out.


<div class="mermaid text-center">
graph TD
    A[Parse AST] --> B
    B[Collect Trivia] --> C
    C[Apply configuration]
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

The input source code string is converted to an [ISourceText](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-text-isourcetext.html)) internally.

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

### Collect Trivia

A syntax tree contains almost all the information we need to format the code. 
However, there are three items that are either missing all together or require further processing:

- Blank lines
- Code comments
- Conditional directives

These three items are labelled as `Trivia` in Fantomas. We need to restore them because the source code originally had them, but cannot do so purely on the AST.

#### Detecting trivia

Trivia can however be easily detected in Fantomas. Both code comments and conditional directives are present in the AST.
These are stored on the file level (in [ParsedImplFileInput](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-parsedimplfileinput.html) and  [ParsedSigFileInput](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-parsedsigfileinput.html) in the `trivia` node).

In both [ParsedImplFileInputTrivia](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntaxtrivia-parsedimplfileinputtrivia.html) and [ParsedSigFileInputTrivia](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntaxtrivia-parsedsigfileinputtrivia.html) we can see comments and conditional directives.

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
We do this in `Trivia.collectTrivia`.

Note: blank lines are detected differently, we go over all the lines via the `ISourceText`.

#### Assigning trivia

Once we have the trivia, we can assign them to an AST node they belong to.
This is one of the more tricky parts of the process.

The syntax tree exists of numerous types of nodes. There is not one discriminated union that captures all the nodes.
Top level nodes are [SynModuleDecl](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synmoduledecl.html), expressions are [SynExpr](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html) and so on.   
In Fantomas all these types do share a comment trait, they can (almost) all have trivia that belongs to them.

Example:

```fsharp
// comment 1
module Hey

// comment 2
let x y = // comment 3
    // comment 4
    y = 1
```

All these comments need to be assigned to the correct node. From the trivia point of view, the owner could be of any type.  
That is why we process the AST into a more generic tree structure called `TriviaNode`.   
A `TriviaNode` always has a type field of `FsAstType` and could have child `TriviaNode`s.  
`FsAstType` is discriminated union that represents all the useful types of AST nodes, regardless of the concrete type they belong too.  
You can see in `TriviaTypes` that this is a long and flat list of all the types.

We construct the root tree of `TriviaNode` in `ASTTransformer`. There we go over the AST and create a tree of `TriviaNode`s.

```fsharp
let a = 
   // comment b
   c
```

would be transformed to

```text
ParsedInput_: tmp.fsx (2,0--4,4)
  SynModuleOrNamespace_AnonModule: tmp.fsx (2,0--4,4)
    SynModuleDecl_Let: tmp.fsx (2,0--4,4)
      SynBindingKind_Normal: tmp.fsx (2,4--4,4)
        SynPat_Named: tmp.fsx (2,4--2,5)
          SynIdent_: tmp.fsx (2,4--2,5)
        SynBinding_Equals: tmp.fsx (2,6--2,7)
        SynExpr_Ident: tmp.fsx (4,3--4,4)
          Ident_: tmp.fsx (4,3--4,4)
```

We use this custom tree to assign the trivia to the correct node.
Take our `//comment b` for example, we will assign it to the `SynExpr_Ident` node.

There are different strategies to assign the trivia to a `TriviaNode`, these can be found in the `Trivia` module.  
Once we know that the code comment belongs to the ident, we store this information in a `TriviaInstruction`.

```fsharp
{
    Trivia = { Item = TriviaContent.Comment (Comment.CommentOnSingleLine "// comment b")
    Type = FsAstType.SynExpr_Ident
    Range = [4,3--4,4]
    AddBefore = true
}
```

Lastly, all the instructions are stored in the `Context` record.  

To summarize, the `Context` initialization:

<div class="mermaid text-center">
graph TD
    A[Transform AST to TriviaNode tree] --> B
    B[Capture all trivia from AST and ISourceText] --> C
    C[Assign trivia to TriviaNode and create TriviaInstructions]
 </div>

<div class="d-flex justify-content-between my-4">
  <a href="./Solution%20Structure.html">Previous</a>
  <a href="./Print%20AST%20with%20Context.html">Next</a>
</div>