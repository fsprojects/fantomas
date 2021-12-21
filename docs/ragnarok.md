# The Ragnarok Feature

I'm not sure who this document is for right now. These are some thoughts and some research about a certain feature that people want in Fantomas.
The feature is about how the code is printed back to source and is a variation on what the style guide advices today.

I will go in great lengths why this is feature does not bring much value to the overall mission of Fantomas.
Why I consider it inconsistent and why the implementation is far from trivial.
Throughout this document there will be a negative tone toward this and for the initial draft I'm ok with this.
Again, there are no plans to publish this as is.
Oh and typos all over the place.

## Introduction

The feature has been request multiple times:
- https://github.com/fsprojects/fantomas/issues/1408
- https://github.com/fsprojects/fantomas/issues/1225
- https://github.com/fsprojects/fantomas/issues/453 (in comments)

The gist is that some multiline expressions should start on the same line to save some space:

```fsharp
let v = {
    X = x
    Y = y
}
```

The style guides deal with this by putting the entire expression on the next line:

```fsharp
let v =
    { X = x
      Y = y }

// or

let v =
    {
        X = x
        Y = y
    }
```

## The inconsistency

When you dissect the initial sample in AST you get something like:

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
                  PreXmlDoc ((1,4), FSharp.Compiler.Xml.XmlDocCollector),
                  SynValData
                    (None, SynValInfo ([], SynArgInfo ([], false, None)), None),
                  Named (v, false, None, tmp.fsx (1,4--1,5)), None,
                  Record
                    (None, None,
                     [((LongIdentWithDots ([X], []), true), Some (Ident x),
                       Some (tmp.fsx (2,10--3,4), None));
                      ((LongIdentWithDots ([Y], []), true), Some (Ident y), None)],
                     tmp.fsx (1,8--4,1)), tmp.fsx (1,4--1,5),
                  Yes tmp.fsx (1,0--4,1))], tmp.fsx (1,0--4,1))], PreXmlDocEmpty,
          [], None, tmp.fsx (1,0--4,1))], (true, true)))
```

or simplified:

`SynBinding(pat = pat; expr = expr)`

The `pat` represents the `v` and the `expr` everything after the equals sign.
In Fantomas we adhere to a simple rule, we tried an put everything on one line `let v = { X = ...` and if that crossed a certain threshold (based on a setting), we put the expression on the next line indented.

Now the ask for the ragnarok setting, is to not do this for a handful of [SynExpr](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html).
There are about 65 union cases for `SynExpr` and for maybe 5 cases, people want to deviate from our currently consistent behavior.

Of course, the thing is that the formatting of these `SynExpr` depends on the context of where the nodes are in.
Example:

```fsharp
let v = {
    X = x
    Y = y
}

// versus

let vlist = [
    {  X = x
       Y = y  }
    // or
    {
        X = x
        Y = y
    }
]
```

If `SynExpr.Record` is the `expr` in a `SynBinding`, if would not require a newline after the `=` to start.
If it is part of `SynExpr.ArrayOrListComputed`, it would be following the default rules I guess.
Point is that, the combination of two syntax nodes would lead to a different style and that will occur all over the SyntaxTree.

## The subjectivity

As a long term Fantomas user, over time you stop caring about how the code looks like. You accept what is does and letting go of your past habits leads to a world of freedom.
People that do not use Fantomas, cannot cope with the fact that the formatted code does differ from their original source.
That is the idea thought, you follow a style guide and your code looks like how the rest of the world does it.

In any case, as a maintainer, I'm always caught in between giving the people what they want and giving them what they need.
The point I'm trying to make is that there is no right or wrong in the style of code. If you prefer your own handwriting that is fine, but using a typewriter works just as well to bring your story.

So, asking for a new style without any solid arguments really is a hard sell. People mentioned that this is a popular style and all that jazz but never bring up any numbers.
Nor, do they understand the technical nature of what their preferred style implies.
And lastly, not a single person has engaged the discussion in the MS style guide. This really rubs me the wrong way.
People want something, don't have a solid case, are clueless and don't put in the proper legwork to get somewhere.

## Scope

There are many syntax nodes in play for this feature.
Most people only list one example when they ask for this feature, but the realm of the SyntaxTree can be a quite large one.

### SynExpr

The `SynExpr` I believe that should be included in this would be:

- [SynExpr.Record](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#Record)
- [SynExpr.AnonRecd](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#AnonRecd)
- [SynExpr.ComputationExpr](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#ComputationExpr)
- [SynExpr.ArrayOrListComputed](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#ArrayOrListComputed)

I'm not sure that this list is completed.
Some people might also include [SynExpr.MatchLambda](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#MatchLambda).
And I also wonder about tuples, [SynExpr.Tuple](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#Tuple).

Maybe multiline strings ([SynConst.String](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synconst.html#String) in [SynExpr.Const](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#Const)) should also be included:

```fsharp
let json = """
{
    "foo":"bar"
}
"""
```

Note that depending on the information stored in these nodes, they are formatted somewhat differently.

### SynPat

The `SynPat` cases might be:
- [SynPat.Record](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synpat.html#Record)
- [SynPat.ArrayOrList](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synpat.html#ArrayOrList)

### SynType

The `SynType` cases might be:
- [SynType.AnonRecd](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-syntype.html#AnonRecd)

### SynBinding

SynBinding is used for let bindings and members:

```fsharp
let x a b = async {
    return a + b
}

type Foo() =
    member this.Bar = {|
        bar with X = x 
    |} // this is quite interesting how the closing brace is indented.
```

Fantomas has different rules depending on the details of the SynBinding.

```fsharp
let a = {
    X = x
}

// different code path in CodePrinter
let b c = {
    X = x
}

// also a different code path in CodePrinter
let d e : MyRecord = {
    X = x
}
```

### LetOrUseBang

Note that not every time the `let` keyword is used, it leads to a `SynBinding`.
[LetBang](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#LetOrUseBang) for example has a different way of storing information.

```fsharp
async {
    let! a = {
        X = x
    }
    ()
}
```

### YieldOrReturn

[YieldOrReturn](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#YieldOrReturn)

```fsharp
myComp {
    yield {
        X = y
    }
    return {
        Y = y
    }
}
```

### YieldReturnFrom

[YieldReturnFrom](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#YieldOrReturnFrom)

```fsharp
myComp {
    yield! {
        X = y
    }
    return! {
        Y = y
    }
}
```

### SynExprAndBang

[SynExprAndBang](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexprandbang.html)

```fsharp
async {
    let! x = y
    and! z = async {
        return! meh
    }
    ()
}
```

### LongIdentSet

[LongIdentSet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#LongIdentSet)

```fsharp
myMutable <- {
    X = x
}
```

### DotIndexedSet

[DotIndexedSet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#DotIndexedSet)

```fsharp
myMutable.[x] <- {
    X = x
}
```

### Set

[Set](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#Set)

```fsharp
myMutable[x] <- {
    X = x
}
```

New F# 6 syntax.

### DotSet

[DotSet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#DotSet)

```fsharp
myMutable().foo <- {
    X = x
}
```

### Lambda

[Lambda](https://fsprojects.github.io/fantomas-tools/#/ast?data=N4KABGBEAmCmBmBLAdrAzpAXFSAacUiaAYmolmPAIYA2as%2BEkAxgPZwWTwCuyYAHmAC0APjBU0AT2TMwwADoAneXwhhFsAC7dFffkpUBfSCENA)

```fsharp
fun x -> async {
    return x
}
```

This is an interesting one as there are quite some rules to format lambda in Fantomas.
There is the raw lambda as you see it above but it is often capture in more elaborate patterns:

```fsharp
myTasks
|> List.map (fun p -> task {
    return p
})
|> Task.WhenAll
```

### SynMatchClause

Used as part of [SynExpr.Match](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#Match) and [SynExpr.MatchBang](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#MatchBang).
[SynMatchClause](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synmatchclause.html).

```fsharp
match v with
| () -> async {
    return FooBar()
}
```

Keep in mind that [SynExpr.TryWith](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#TryWith) has this as well:

```fsharp
try x with
| ex -> async {
    ()
}
```

### App

Another very interesting case, where do you draw the line with [SynExpr.App](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#App)

```fsharp
let v =
    foo [
        a
        b
        c
    ]
```

This is partially already implemented in the Elmish settings.
However, there are again a lot of possibilities there:

```fsharp
let v =
    foo "string" [
        a
        b
        c
    ]
```

this is currently not supported.
When do you draw the line and go over to:

```fsharp
let v =
    foo 
        "string" 
        [ a
          b
          c ]
```

?

#### Named arguments

Another interesting edge case is named arguments inside applications:

```fsharp
let v =
    SomeConstructor(v = [
        A
        B
        C
    ])
```

Note that the AST for `v = [ ... ]` is something like

```fsharp
App
  (NonAtomic, false,
   App
     (NonAtomic, true, Ident op_Equality, Ident v,
      tmp.fsx (2,20--2,23)),
   ArrayOrListComputed
     (false,
      Sequential
        (SuppressNeither, true, Ident A,
         Sequential
           (SuppressNeither, true, Ident B, Ident C,
            tmp.fsx (4,8--5,9)), tmp.fsx (3,8--5,9)),
      tmp.fsx (2,24--6,5)), tmp.fsx (2,20--6,5)),
tmp.fsx (2,19--2,20), Some tmp.fsx (6,5--6,6),
tmp.fsx (2,19--6,6)
```

so this is hard to detect in the first place.

### SynExprRecordField

[SynExprRecordField](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexprrecordfield.html)

```fsharp
let v = {
    X = {
        Y = y
    }
}
```

### Fields in AnonRecords

[recordFields in AnonRecd](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#AnonRecd)

```fsharp
let v = {|
    X = {|
        Y = y
    |}
|}
```

### SynTypeDefnSimpleRepr.Record

[SynTypeDefnSimpleRepr.Record](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-syntypedefnsimplerepr.html#Record)

```fsharp
type V = {
    X :int
    Y: int
}
```

Note, access modifiers:

```fsharp
type V = internal {
    X :int
    Y: int
}
```

Members need the `with` keyword:

```fsharp
type V = {
    X :int
    Y: int
} with
    member this.XY = X + Y
```

The current style does not use the `with` keyword, here is would be a requirement.

This is also being used in [SynTypeDefnSigRepr.Simple](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-syntypedefnsigrepr.html).

```fsharp
namespace Meh

type V = {
    X :int
    Y: int
} with
    member XY : int
```

### TypeAbbrev

[SynType.AnonRecd](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-syntype.html#AnonRecd) in [SynTypeDefnSimpleRepr.TypeAbbrev](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-syntypedefnsimplerepr.html#TypeAbbrev).

```fsharp
type V = {|
    x :int
|}
```

### SynArgPats.NamePatPairs

[SynArgPats.NamePatPairs](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synargpats.html#NamePatPairs)

```fsharp
match x with
| Foo(x = {
    Y = y
  }) ->
     ()
```

### SynPat.Record

[fieldPats in SynPat.Record](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synpat.html#Record)

```fsharp
match x with
| { Y = {
       X = y
    }} ->
     ()
     
// no idea if this looks ok but you get the idea, after the `Y =` you have the scenario.
```

### Even more nodes

I'm quite certain that the list above is not complete.

## Implementation

The impact will be huge in CodePrinter, there are numerous locations where some clever helper function will need to be called in order not to newline.
I do believe that not placing the newline will not be the only thing that is required to make all these examples work.
Having an entirely new implementation for all the impacted nodes is also not recommended.

Some re-use might be possible on the record side by turning on [fsharp_multiline_block_brackets_on_same_column](https://github.com/fsprojects/fantomas/blob/master/docs/Documentation.md#fsharp_multiline_block_brackets_on_same_column).
However, by doing this, a new precedent will be introduced. Two settings need to be combined in order for a valid code to be outputted.
This is unseen for the tool.

And no battle plan survives first contact. Even if everything above is implemented and it produces no warnings whatsoever, the will more definitely be a case that pops up once this is released in the wild.

## The twist

Even though this whole thing is a bad idea, like a really bad one, I might be open to it in the future.
There are two things I still wish to achieve in the Fantomas project:
- A better Syntax tree: improvements on the compiler side to simplify Fantomas
- Parallel formatting: formatting certain syntax tree nodes in parallel to speed up things for large files.

After that, I'm willing to open a bit to what the community wants out of this project.
I might even agreed to the ragnarok feature under very strict conditions.
These obviously would be that the feature is not breaking any existing tests and is not impacting anything else whatsoever.

I'm also pretty much not going to do this implementation myself unless I'm properly paid for it.
Again, I don't care and this is a bad idea.