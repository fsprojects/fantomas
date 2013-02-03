F# Formatting Conventions
===

This article is written mostly based on ["F# Coding Guidelines"](http://research.microsoft.com/fsharp/expert-fsharp-draft/FormattingConventions.doc) from Don Syme.
There are certain bits of the original documents which need to be updated when F# has changed a lot in last few years.
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

#### Tuples, records, lists and arrays ####

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

#### Records #####
Short records can be written in one line:
```fsharp
let point = { X = 1.0; Y = 0.0 }
```

Opening token for records starts in a new line. Closing token is normally on end of line of last construct:

```fsharp
let rainbow = 
    { boss  = "Jeffrey"; 
      lackeys = ["Zippy"; "George"; "Bungle"] }
```

Not everyone likes this style, and variation is OK. 
For large constructs (> 6 lines) the closing token can be on a fresh line:

```fsharp
let rainbow = 
    { boss1  = "Jeffrey"; 
      boss2  = "Jeffrey"; 
      boss3  = "Jeffrey"; 
      boss4  = "Jeffrey"; 
      boss5  = "Jeffrey"; 
      boss6  = "Jeffrey"; 
      boss7  = "Jeffrey"; 
      boss8  = "Jeffrey"; 
      lackeys = ["Zippy"; "George"; "Bungle"];
    }
```


Note that you can optionally include a trailing `;` for the last entry.


##### Lists and arrays #####
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

but this isn't:

```fsharp
let comparer = 
    { new IComparer<string> with 
      member x.Compare(s1, s2) = 
          let rev (s : String) = 
              new String(Array.rev (s.ToCharArray())) in
              let reversed = rev s1 in 
              reversed.CompareTo(rev s2) }
```
---

#### `if...then...else` ####

---

#### `match` and `try/with` ####

 - Rules of a `with` in a `try/with` can be *optionally* 4-space indented e.g.

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
 - Use a `|` for each clause of a match (strictly speaking it is optional for the first), except when the match is all on one line.

    ```fsharp
    // OK
    match l with
    | { him = x ; her = "Posh" } :: tail -> x
    | _ :: tail -> findDavid tail
    | [] -> failwith "Couldn't find David"


     // Not OK
     match l with
         | { him = x ; her = "Posh" } :: tail -> x
         | _ :: tail -> findDavid tail
         | [] -> failwith "Couldn't find David"

     // OK
     match l with [] -> false | _ :: _ -> true
    ```
    
---

#### Function applications ####

---

#### Value declarations ####

----

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
