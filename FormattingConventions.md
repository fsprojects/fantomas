F# Formatting Conventions
===

This article is written mostly based on ["F# Coding Guidelines"][1] (offline version) from Don Syme.
There are certain bits of the original document that need to be updated when F# has changed a lot in last few years.
Therefore, I attempt to reintroduce F# Formatting Conventions here and add some relevant information from other sources as well.
Another purpose of the article is to recognize requirements for an F# code tidy tool I would like to create.

---

### General rules for indentation ###

---

### Formatting rules for syntactic constructs ###


#### Type definitions ####

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

---

#### Value declarations ####

##### Tuples #####
A tuple is parenthesized and the commas therein (delimiters) are each followed by a space e.g. `(1, 2)`, `(x, y, z)`.

A *commonly accepted exception* is to omit commas in pattern matching of tuples.
The justification is to match multiple values, not construct new tuples.

```fsharp
let x, y = z

match x, y with
| 1, _ -> 0
| x, 1 -> 0
| x, y -> 1
```      

##### Records ######
Short records can be written in one line:
```fsharp
let point = { X = 1.0; Y = 0.0 }
```

Opening token for records starts in a new line. Closing token is normally on end of line of last construct:

```fsharp
let rainbow = 
    { boss = "Jeffrey"; 
      lackeys = ["Zippy"; "George"; "Bungle"] }
```

Not everyone likes this style, and variation is OK. 
For large constructs (> 6 lines) the closing token can be on a fresh line:

```fsharp
let rainbow = 
    { boss1 = "Jeffrey"; 
      boss2 = "Jeffrey"; 
      boss3 = "Jeffrey"; 
      boss4 = "Jeffrey"; 
      boss5 = "Jeffrey"; 
      boss6 = "Jeffrey"; 
      boss7 = "Jeffrey"; 
      boss8 = "Jeffrey"; 
      lackeys = ["Zippy"; "George"; "Bungle"];
    }
```


Note that you can optionally include a trailing `;` for the last entry.

##### Lists and arrays #####
Write `x :: l` with spaces around the `::` (since `::` is an infix operator, hence surrounded by spaces) and `[1; 2; 3]` (since `;` is a delimiter, hence followed by a space).

Always use at least one space between two distinct parantherical operators (e.g. leave a space between a `[` and a `{`).

```fsharp
// OK
[ { IngredientName = "Green beans"; Quantity = 250 }; 
  { IngredientName = "Pine nuts"; Quantity = 250 };
  { IngredientName = "Feta cheese"; Quantity = 250 };
  { IngredientName = "Olive oil"; Quantity = 10 };
  { IngredientName = "Lemon"; Quantity = 1 } ]

// Not OK
[{ IngredientName = "Green beans"; Quantity = 250 }; 
 { IngredientName = "Pine nuts"; Quantity = 250 };
 { IngredientName = "Feta cheese"; Quantity = 250 };
 { IngredientName = "Olive oil"; Quantity = 10 };
 { IngredientName = "Lemon"; Quantity = 1 }]
```

Lists and arrays that split across multiple lines follow a similar rule as records do:

```fsharp
let pascalsTriangle = 
    [| [|1|];
       [|1; 1|];
       [|1; 2; 1|];
       [|1; 3; 3; 1|];
       [|1; 4; 6; 4; 1|];
       [|1; 5; 10; 10; 5; 1|];
       [|1; 6; 15; 20; 15; 6; 1|];
       [|1; 7; 21; 35; 35; 21; 7; 1|]; 
       [|1; 8; 28; 56; 70; 56; 28; 8; 1|];
    |]
```

##### Discriminated unions #####
DUs that split across multiple lines follow a similar rule:

```fsharp
let tree1 = 
    BinaryNode
        (BinaryNode(BinaryValue 1, BinaryValue 2), 
         BinaryNode(BinaryValue 3, BinaryValue 4))
```
However the following is also acceptable:
```fsharp
let tree1 = 
    BinaryNode( 
        BinaryNode(BinaryValue 1, BinaryValue 2), 
        BinaryNode(BinaryValue 3, BinaryValue 4) 
    )
```

---

#### Object expressions and interfaces ####

Object expressions and interfaces are aligned in the same way with `member` being indented after 4 spaces.
For example, this is recommended:

```fsharp
let comparer = 
    { new IComparer<string> with
          member x.Compare(s1, s2) = 
              let rev (s : String) = 
                  new String(Array.rev (s.ToCharArray())) 
              let reversed = rev s1 i
              reversed.CompareTo(rev s2) }
```

but this isn't advocated:

```fsharp
// Not OK
let comparer = 
    { new IComparer<string> with 
      member x.Compare(s1, s2) = 
          let rev (s : String) = 
              new String(Array.rev (s.ToCharArray())) in
              let reversed = rev s1 in 
              reversed.CompareTo(rev s2) }
```
---

#### Conditional expressions ####

##### Multiple branches #####

Multiple conditionals start each line (except the first one) by the keyword `else` or `elif`:
```fsharp
if cond1 then e1
elif cond2 then e2
elif cond3 then e3
else e4
```

##### Single branches #####

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
---

#### Pattern matching constructs ####

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
This doesn't add any good to readability hence **isn't recommended**.

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
The justification is that it is harder to maintain the program. Adding a new case may screw up indentation and we often give up alignment at that time.

---

#### Function applications ####

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

---

#### Infix operators ####
Be careful to *keep operator symbols well separated by spaces*: not only will your formulas be more readable, but you will avoid confusion with multi-character operators. 
Obvious exceptions to this rule, the symbols `!` and `.`, are not separated from their arguments.
Moreover, infix expressions are ok to lineup on same column:
```fsharp
// OK
   acc + 
   (Printf.sprintf "\t%s - %i\n\r" 
        x.IngredientName x.Quantity)
        
// OK        
let function1 arg1 arg2 arg3 arg4 =
    arg1 + arg2
  + arg3 + arg4
```

---

#### Pipeline operators ####

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

---

#### Computation expressions ####

---

### Conclusions ###

---

### References ###
This document is structured upon ["F# Coding Guidelines"][1] (offline version).
General rules for indentation are referenced at ["Code Formatting Guidelines (F#)"][2].
A few conventions for syntactic constructs are adapted from ["Caml Programming Guidelines"][3].
Other whitespace-significant rules are taken from ["PEP 8 -- Style Guide for Python Code"][4].

  [1]: http://research.microsoft.com/fsharp/expert-fsharp-draft/FormattingConventions.doc
  [2]: http://msdn.microsoft.com/en-us/library/dd233191.aspx
  [3]: http://caml.inria.fr/resources/doc/guides/guidelines.en.html
  [4]: http://www.python.org/dev/peps/pep-0008/
