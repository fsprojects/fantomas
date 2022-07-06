---
category: Documentation
categoryindex: 1
index: 6
---
# Style guide
A comprehensive guide to F# Formatting Conventions
===

This article is written mostly based on ["F# Coding Guidelines"][1] (offline version) from Don Syme.
There are certain bits of the original document that need to be updated when F# has changed a lot in last few years.
Therefore, I attempt to reintroduce F# Formatting Conventions here and add some relevant information from other sources as well.
Another purpose of the article is to recognize requirements for an F# source code formatter I would like to create.

## **Table of Contents**

- [General rules for indentation](#General-rules-for-indentation)
	- [Using spaces](#Using-spaces)
	- [Offside rule](#Offside-rule)
- [Formatting rules for syntactic constructs](#Formatting-rules-for-syntactic-constructs)
	- [Type definitions](#Type-definitions)
	- [Value declarations](#Value-declarations)
		- [Tuples](#Tuples)
		- [Records](#Records)
		- [Lists and arrays](#Lists-and-arrays)
		- [Discriminated unions](#Discriminated-unions)
	- [Conditional expressions](#Conditional-expressions)
		- [Multiple branches](#Multiple-branches)
		- [Single branches](#Single-branches)
	- [Pattern matching constructs](#Pattern-matching-constructs)
	- [Function applications](#Function-applications)
	- [Infix operators](#Infix-operators)
	- [Pipeline operators](#Pipeline-operators)
	- [Modules](#Modules)
	- [Object expressions and interfaces](#Object-expressions-and-interfaces)
	- [Whitespaces](#Whitespaces)
	- [Blank lines](#Blank-lines)
	- [Comments](#Comments)
- [Conclusions](#Conclusions)
- [References](#References)

## General rules for indentation

### Using spaces
When indentation is required, you must use spaces, not tabs. 
At least one space is required. 
Your organization can create coding standards to specify the number of spaces to use for indentation; two, three or four spaces of indentation at each level where indentation occurs is typical. 
That said, indentation of programs is a subjective matter.
Variations are OK, but the first rule you should follow is *consistency of indentation*:

> Choose a generally accepted style of indentation, then use it systematically throughout the whole application.

You can configure Visual Studio to match your organization's indentation standards by changing the options in the **Options** dialog box, which is available from the **Tools** menu. 
In the **Text Editor** node, expand **F#** and then click **Tabs**. For a description of the available options, see [Options, Text Editor, All Languages, Tabs](http://msdn.microsoft.com/en-us/library/7sffa753.aspx).

In general, when the compiler parses your code, it maintains an internal stack that indicates the current level of nesting. 
When code is indented, a new level of nesting is created, or pushed onto this internal stack. 
When a construct ends, the level is popped. 
Indentation is one way to signal the end of a level and pop the internal stack, but certain tokens also cause the level to be popped, such as the `end` keyword, or a closing brace or parenthesis.

### Offside rule
A page is often 80 columns wide.
Code in a multiline construct, such as a type definition, function definition, `try...with` construct, and looping constructs, must be indented relative to the opening line of the construct. 
The first indented line establishes a column position for subsequent code in the same construct. 
The indentation level is called a *context*. The column position sets a minimum column, referred to as an *offside line*, for subsequent lines of code that are in the same context. 
When a line of code is encountered that is indented less than this established column position, the compiler assumes that the context has ended and that you are now coding at the next level up, in the previous context. 
The term *offside* is used to describe the condition in which a line of code triggers the end of a construct because it is not indented far enough. 
In other words, code to the left of an offside line is offside. 
In correctly indented code, you take advantage of the offside rule in order to delineate the end of constructs. 
If you use indentation improperly, an offside condition can cause the compiler to issue a warning or can lead to the incorrect interpretation of your code.
Offside lines are determined as follows.
 - An `=` token associated with a let introduces an offside line at the column of the first token after the `=` sign.
 - In an `if...then...else` expression, the column position of the first token after the `then` keyword or the `else` keyword introduces an offside line.
 - In a `try...with` expression, the first token after `try` introduces an offside line.
 - In a `match` expression, the first token after `with` and the first token after each `->` introduce offside lines.
 - The first token after `with` in a type extension introduces an offside line.
 - The first token after an opening brace or parenthesis, or after the `begin` keyword, introduces an offside line.
 - The first character in the keywords `let`, `if`, and `module` introduce offside lines.

## Formatting rules for syntactic constructs
Keep in mind that *code is read much more often than it is written*.
This section introduces a set of recommendations to improve the readability of code.
Consistency with the recommendations is important.
However, sometimes these formatting conventions do not apply.
It is a good reason to break a particular rule, if applying it would make the code less readable.

In this section, code fragments without comments are of good styles.
Bad coding styles will be explicitly specified by corresponding comments.
Although I also use 4 spaces as the indentation standard, all the rules are equally applied for 2, 3 spaces, etc.

### Type definitions

Indent `|` in type definition by 4 spaces:

```fsharp
// OK
type Volume = 
    | Liter of float
    | USPint of float
    | ImperialPint of float

// Not OK
type Volume = 
| Liter of float
| USPint of float
| ImperialPint of float
```

### Value declarations

#### Tuples
A tuple is parenthesized and the commas therein (delimiters) are each followed by a space e.g. `(1, 2)`, `(x, y, z)`.

A *commonly accepted exception* is to omit parentheses in pattern matching of tuples.
The justification is to match multiple values, not construct new tuples.

```fsharp
let x, y = z

match x, y with
| 1, _ -> 0
| x, 1 -> 0
| x, y -> 1
```      

#### Records
Short records can be written in one line:
```fsharp
let point = { X = 1.0; Y = 0.0 }
```

Opening token for records starts in a new line. Closing token is normally on the end of line of last construct:

```fsharp
let rainbow = 
    { boss = "Jeffrey"
      lackeys = ["Zippy"; "George"; "Bungle"] }
```

Not everyone likes this style, and variation is ok. 
For large constructs (> 6 lines) the closing token can be on a fresh line:

```fsharp
let rainbow = 
    { boss1 = "Jeffrey"
      boss2 = "Jeffrey" 
      boss3 = "Jeffrey" 
      boss4 = "Jeffrey" 
      boss5 = "Jeffrey" 
      boss6 = "Jeffrey" 
      boss7 = "Jeffrey" 
      boss8 = "Jeffrey" 
      lackeys = ["Zippy"; "George"; "Bungle"]
    }
```

Assume that all fields are aligned at the same column, the trailing `;` right before each line break is optional.
You can also optionally include a trailing `;` for the last entry.
The same rule applies for list and array elements.

#### Lists and arrays
Write `x :: l` with spaces around the `::` operator (`::` is an infix operator, hence surrounded by spaces) and `[1; 2; 3]` (`;` is a delimiter, hence followed by a space).

Always use at least one space between two distinct parenthetical operators (e.g. leave a space between a `[` and a `{`).

```fsharp
// OK
[ { IngredientName = "Green beans"; Quantity = 250 }
  { IngredientName = "Pine nuts"; Quantity = 250 }
  { IngredientName = "Feta cheese"; Quantity = 250 }
  { IngredientName = "Olive oil"; Quantity = 10 }
  { IngredientName = "Lemon"; Quantity = 1 } ]

// Not OK
[{ IngredientName = "Green beans"; Quantity = 250 }
 { IngredientName = "Pine nuts"; Quantity = 250 }
 { IngredientName = "Feta cheese"; Quantity = 250 }
 { IngredientName = "Olive oil"; Quantity = 10 }
 { IngredientName = "Lemon"; Quantity = 1 }]
```

Lists and arrays that split across multiple lines follow a similar rule as records do:

```fsharp
let pascalsTriangle = 
    [| [|1|]
       [|1; 1|]
       [|1; 2; 1|]
       [|1; 3; 3; 1|]
       [|1; 4; 6; 4; 1|]
       [|1; 5; 10; 10; 5; 1|]
       [|1; 6; 15; 20; 15; 6; 1|]
       [|1; 7; 21; 35; 35; 21; 7; 1|] 
       [|1; 8; 28; 56; 70; 56; 28; 8; 1|]
    |]
```

#### Discriminated unions
DUs that split across multiple lines follow a similar rule:

```fsharp
let tree1 = 
    BinaryNode
        (BinaryNode(BinaryValue 1, BinaryValue 2), 
         BinaryNode(BinaryValue 3, BinaryValue 4))
```
However, the following way is also acceptable:
```fsharp
let tree1 = 
    BinaryNode( 
        BinaryNode(BinaryValue 1, BinaryValue 2), 
        BinaryNode(BinaryValue 3, BinaryValue 4) 
    )
```

### Conditional expressions

#### Multiple branches

Multiple conditionals open each line counting from the second one by the keyword `else` or `elif`:
```fsharp
if cond1 then e1
elif cond2 then e2
elif cond3 then e3
else e4
```

#### Single branches

Indentation of conditionals depends on the sizes of the expressions which make them up.
If `cond`, `e1` and `e2` are small, simply write them on one line:
```fsharp
if cond then e1 else e2
```
If `e1` and `cond` are small, but `e2` is large:
```fsharp
if cond then e1
else
    e2
```
If `e1` and `cond` are large and `e2` is small:
```fsharp
if cond then
    e1
else e2
```
If all the expressions are large:
```fsharp
if cond then
    e1
else
    e2
```

### Pattern matching constructs

Rules of a `with` in a `try/with` can be *optionally* 4-space indented e.g.

```fsharp
try
    if System.DateTime.Now.Second % 3 = 0 then
        raise (new System.Exception())
    else
        raise (new System.ApplicationException())
with
| :? System.ApplicationException -> 
    printfn "A second that was not a multiple of 3"    
| _ -> 
    printfn "A second that was a multiple of 3"
```

but this is also OK:

```fsharp
try
    if System.DateTime.Now.Second % 3 = 0 then
        raise (new System.Exception())
    else
        raise (new System.ApplicationException())
with
    | :? System.ApplicationException -> 
        printfn "A second that was not a multiple of 3"    
    | _ -> 
        printfn "A second that was a multiple of 3"
```

Use a `|` for each clause of a match (strictly speaking it is optional for the first), except when the match is all on one line.

```fsharp
// OK
match l with
| { him = x; her = "Posh" } :: tail -> x
| _ :: tail -> findDavid tail
| [] -> failwith "Couldn't find David"

// Not OK
match l with
    | { him = x; her = "Posh" } :: tail -> x
    | _ :: tail -> findDavid tail
    | [] -> failwith "Couldn't find David"

// OK
match l with [] -> false | _ :: _ -> true
```

If the expression on the right of the pattern matching arrow is too large, cut the line after the arrow.
```fsharp
match lam with
| Abs(x, body) ->
   1 + sizeLambda body
| App(lam1, lam2) ->
   sizeLambda lam1 + sizeLambda lam2
| Var v -> 1
```
Some programmers apply this rule systematically to any clause of any pattern matching. 
This does not add any good to readability hence **is not recommended**.

```fsharp
// Not OK
let rec fib = function
    | 0 ->
        1
    | 1 ->
        1
    | n ->
        fib (n - 1) + fib (n - 2)
```       
Pattern matching of anonymous functions, starting by `function`, are indented with respect to the `function` keyword:

```fsharp
List.map (function
          | Abs(x, body) -> 1 + sizeLambda 0 body
          | App(lam1, lam2) -> sizeLambda (sizeLambda 0 lam1) lam2
          | Var v -> 1) lambdaList
```

Pattern matching in functions defined by `let` or `let rec` are indented 4 spaces after starting of `let` although `function` keyword may be used:
```fsharp
let rec sizeLambda acc = function
    | Abs(x, body) -> sizeLambda (succ acc) body
    | App(lam1, lam2) -> sizeLambda (sizeLambda acc lam1) lam2
    | Var v -> succ acc
```

**Careful alignment of the arrows** of a pattern matching is considered **bad practice**, as exemplify in the following fragment:
```fsharp
// Not OK
let f = function
    | C1          -> 1
    | LongName _  -> 2
    | _           -> 3
```
The justification is that it is harder to maintain the program. 
Adding a new case may screw up indentation and we often give up alignment at that time.

### Function applications

Arguments are always indented from functions:

```fsharp
// OK
Printf.sprintf "\t%s - %i\n\r" 
     x.IngredientName x.Quantity

// OK
Printf.sprintf
     "\t%s - %i\n\r" 
     x.IngredientName x.Quantity 

// OK
let printVolumes x = 
    Printf.printf "Volume in liters = %f, in us pints = %f, in imperial = %f" 
        (convertVolumeToLiter x) 
        (convertVolumeUSPint x) 
        (convertVolumeImperialPint x) 

// Not OK
let printVolumes x = 
    Printf.printf "Volume in liters = %f, in us pints = %f, in imperial = %f" 
    (convertVolumeToLiter x) 
    (convertVolumeUSPint x) 
    (convertVolumeImperialPint x) 

// Not OK
Printf.sprintf "\t%s - %i\n\r" 
x.IngredientName x.Quantity
```

Anonymous function arguments can be either on next line or with a dangling `fun` on the argument line:

```fsharp
// OK
let printListWithOffset a list1 =
    List.iter (fun elem ->
        printfn "%d" (a + elem)) list1

// Tolerable
let printListWithOffset a list1 =
    List.iter (
        fun elem ->
            printfn "%d" (a + elem)) list1
```

### Infix operators
Be careful to *keep operator symbols well separated by spaces*; not only will your formulas be more readable, but you will avoid confusion with multi-character operators. 
Obvious exceptions to this rule are the `!` and `.` symbols.
They are not separated from their arguments.
Moreover, infix expressions are OK to lineup on same column:
```fsharp
acc + 
(Printf.sprintf "\t%s - %i\n\r" 
     x.IngredientName x.Quantity)
            
let function1 arg1 arg2 arg3 arg4 =
    arg1 + arg2
  + arg3 + arg4
```

### Pipeline operators

Pipeline `|>` should go at the start of a line immediately under the expression being operated on:

```fsharp
// OK
let methods2 = 
    System.AppDomain.CurrentDomain.GetAssemblies()
    |> List.ofArray 
    |> List.map (fun assm -> assm.GetTypes()) 
    |> Array.concat
    |> List.ofArray 
    |> List.map (fun t -> t.GetMethods()) 
    |> Array.concat

// OK
let methods2 = System.AppDomain.CurrentDomain.GetAssemblies()
               |> List.ofArray 
               |> List.map (fun assm -> assm.GetTypes()) 
               |> Array.concat
               |> List.ofArray 
               |> List.map (fun t -> t.GetMethods()) 
               |> Array.concat

// Not OK
let methods2 = System.AppDomain.CurrentDomain.GetAssemblies()
            |> List.ofArray 
            |> List.map (fun assm -> assm.GetTypes()) 
            |> Array.concat
            |> List.ofArray 
            |> List.map (fun t -> t.GetMethods()) 
            |> Array.concat
```

### Modules
Code in a local module must be indented relative to the module, but code in a top-level module does not have to be indented. 
Namespace elements do not have to be indented.
The following code examples illustrate this.
```fsharp
// A is a top-level module. 
module A

let function1 a b = a - b * b
```

```fsharp
// A1 and A2 are local modules. 
module A1 =
    let function1 a b = a*a + b*b

module A2 =
    let function2 a b = a*a - b*b
```

### Object expressions and interfaces

Object expressions and interfaces are aligned in the same way with `member` being indented after 4 spaces.
For example, this is recommended:

```fsharp
let comparer = 
    { new IComparer<string> with
          member x.Compare(s1, s2) = 
              let rev (s : String) = 
                  new String (Array.rev (s.ToCharArray())) 
              let reversed = rev s1 i
              reversed.CompareTo (rev s2) }
```

but this is not advocated:

```fsharp
// Not OK
let comparer = 
    { new IComparer<string> with 
      member x.Compare(s1, s2) = 
          let rev (s : String) = 
              new String (Array.rev (s.ToCharArray())) in
              let reversed = rev s1 in 
              reversed.CompareTo (rev s2) }
```
Bodies of modules, classes, interfaces, and structures delimited by `begin...end`, `{...}`, `class...end`, or `interface...end`. 
This allows for a style in which the opening keyword of a type definition can be on the same line as the type name without forcing the whole body to be indented further than the opening keyword.
```fsharp
type IMyInterface = interface 
    abstract Function1 : int -> int
end
```

### Whitespaces

Avoid extraneous whitespace in the following situations:

 - Immediately inside parentheses and brackets.
   
   ```fsharp
   // OK 
   spam (ham.[1])
   
   // Not OK
   spam ( ham.[ 1 ] )
   ```
 - Immediately before a comma and semicolon.
 - Around the `=` sign when used to indicate a named argument.
   ```fsharp
   // OK
   let makeStreamReader x = new System.IO.StreamReader(path=x)
   
   // Not OK
   let makeStreamReader x = new System.IO.StreamReader(path = x)
   ```

### Blank lines
 - Separate top-level function and class definitions with two blank lines.
 - Method definitions inside a class are separated by a single blank line.
 - Extra blank lines may be used (sparingly) to separate groups of related functions. Blank lines may be omitted between a bunch of related one-liners (e.g. a set of dummy implementations).
 - Use blank lines in functions, sparingly, to indicate logical sections.
 
#### 2020 Revision

Blank lines are introduces around any multiline code constructs:
```fsharp
let a = 9

if someCondition then
    printfn "meh"
    ()

let b = 10
let c = 10
```

The `SynExpr.IfThenElse` expression is multiline so a blank line between `let a` and `if someCondition` and between `if someCondition` and `let b` is fitting.
Single line statements are combined without any additional blank lines, see `let b` and `let c`. 

### Comments

Block comments generally apply to some (or all) code that follows them, and are indented to the same level as that code. 
Each line of a block comment starts with a `(*` or `//` and a single space (unless it is indented text inside the comment).
Paragraphs inside a block comment are separated by a line containing a single `*` or `//`.

Use inline comments sparingly.
An inline comment is a comment on the same line as a statement. 
Inline comments should be separated by at least two spaces from the statement. 
They should start with a `//` and a single space.

## Conclusions
This guideline is still far from complete.
Many syntactic constructs have not had any defined rule yet.
In those cases, please keep in mind *consistency of indentation* rule and extrapolate from rules of similar constructs.
Although I tried to keep a neutral position, there are mistakes and inconsistencies here and there.
Feedbacks and suggestions for improving the article are always welcome.

## References
This document is structured upon ["F# Coding Guidelines"][1] (offline version).
General rules for indentation are referenced at ["Code Formatting Guidelines (F#)"][2].
A few conventions for syntactic constructs are adapted from ["Caml Programming Guidelines"][3].
Other whitespace-significant rules are taken from ["PEP 8 -- Style Guide for Python Code"][4].

  [1]: https://web.archive.org/web/20070306131206/http://research.microsoft.com/fsharp/expert-fsharp-draft/FormattingConventions.doc
  [2]: http://msdn.microsoft.com/en-us/library/dd233191.aspx
  [3]: http://caml.inria.fr/resources/doc/guides/guidelines.en.html
  [4]: http://www.python.org/dev/peps/pep-0008/
