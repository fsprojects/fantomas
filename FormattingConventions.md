F# Formatting Conventions
===

This article is written mostly based on ["F# Coding Guidelines"](http://research.microsoft.com/fsharp/expert-fsharp-draft/FormattingConventions.doc) from Don Syme.
There are certain bits of the original documents which need to be updated when F# has changed a lot in last few years.
Therefore, I attempt to reintroduce F# Formatting Conventions here and add some relevant information from other sources as well.
Another purpose of the article is to realize requirements for an F# code tidy tool I would like to create.

---

### General rules for indentation ###

---

### How to write lists, arrays, tuples and records ###

---

### How to indent object expressions ###

Object expressions and interfaces are aligned in the same way with `member` being indented after 4 spaces.
For example, this is recommended

```fsharp
let comparer = 
    { new IComparer<string> with
          member x.Compare(s1, s2) = 
              let rev (s : String) = 
                  new String(Array.rev (s.ToCharArray())) 
              let reversed = rev s1 i
              reversed.CompareTo(rev s2) }
```

but not this

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

### Formatting rules for `if...then...else` ###

---

### How to write match and try/with ###

---

### How to indent function applications ###

---

### How to write declarations ###

----

### How to indent pipelines ###

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

### How to indent type definitions ###

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
| USPpint of float
| ImperialPint of float
```

---

### How to write comprehensions ###

---

### Conclusions ###

---

### References ###
