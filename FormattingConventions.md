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

#### Lists, arrays, tuples and records ####

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

 - Rules of a “with” in a “try”/”with” can be *optionally* 4-space indented e.g.

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

#### Type definitions ####

Indent `|` in type definition by 4 spaces:

```fsharp
// OK
type volume = 
    | Liter of float
    | USPint of float
    | ImperialPint of float

// Not OK
type volume = 
| Liter of float
| USPint of float
| ImperialPint of float
```

---

#### Computation expressions ####

---

### Conclusions ###

---

### References ###
