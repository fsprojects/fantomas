# Fantomas V

*"Bare Metal Alchemist"*

## Introduction

Fantomas has an overarching theme that spans over all its release. A simple theme to be frank, take any source code and produce something that is valid and consistent.
Although this seems like a very trivial mission statement, it can be damn difficult to nail this one down.

A crucial part of Fantomas, is having an understanding of what happened in the original source code. We use the FSharp Compiler Service (FCS) for solving these mysteries.
Our biggest ally is the parser, which constructs an tree model of the syntax constructs in the source string. 
We walk this tree and try and come up with an entirely new output.

### The struggle of the garden

The untyped syntax tree is not perfect. There are certain nodes missing and others are already optimized so we cannot deduce their original writing anymore.
This is problematic to restore the finer details of the source code. To overcome this, Fantomas uses the F# tokens to gain additional information.
We call these trivia, and besides capturing trivia, we also need to relate them with the syntax tree nodes.
This process is a very challenging one. One that improves over time, but only as new cases have been reported.

### Being the gardener

To solve some of the problems at the root, we can submit PR's to the [F# compiler](https://github.com/dotnet/fsharp) itself.
These are not always easy fixes, but can lead to supreme simplifications at Fantomas side.
In general the F# team is on board with these changes and has been very supportive.

### Shipping a garden is hard

The release cycle of the FCS packages on NuGet is currently tied to cycle of the .NET SDK releases.
So, it can take months before a PR that improves the syntax tree can utilized in Fantomas.
And, from my own experience, it is even more frustrating when some edge case pops up after you tried to integrate the new FCS in Fantomas.

The point I'm trying to make, is to improve the core of Fantomas, it can take forever.
Or that is how this patience-less writer is perceiving it.
This is nobodies fault and I understand why the world is that it is today.
However...

### Planting the seeds of liberty

The major theme of the 4.6 release was the decoupling of Fantomas and the editors.
In short, your editor will now use the command line tool in an efficient way.
You can bring your version of Fantomas and this opens a lot of doors.

We don't need to care anymore about Fantomas (as a dll) needing to fit into each editor tooling system.
In the past, the stars and the FCS versions needed to align in order for new Fantomas versions to get picked up by editors.
Since 4.6, we dropped that restriction and by extent freed ourselves from having to use the public FCS packages on NuGet.

### Using a younger tree

The goal of Fantomas 5 will be to improve the F# syntax tree, while developing both side by side.
In practise, we are going to create our own FCS package based on the source code of the compiler.

The idea is to expose the lexer and the parser from the compiler, at a commit of our choosing.
Meaning we can move forward once a new (relevant) PR is merged to the main branch.

This Fantomas FCS will be more lightweight than the classical FCS that you can find on NuGet.
Because we are only interested in the first phases of the compiler, we can trim a lot code and dependencies and so reduce the footprint.

## Going bare metal

Enough with the garden stuff, let's talk shop. The removal of the `FCS` nuget package leaves a void to fill.
This gets resolved a carefully crafted fsharp project that takes the source files we need from `dotnet/fsharp`.
The plan is to included those via [Paket GitHub dependencies](https://fsprojects.github.io/Paket/github-dependencies.html), using a very specific git commit pointer.

### API layer

As we only are including the files we really really need, we created our own little FCS surface API.
In `Lex.fs` and `Parse.fs` you can find some (mostly stolen) code that serves as the entry point for Fantomas.

### Lexing at the lower level

Our Fantomas FCS lexing api is using what the `*.fsl` produces. These tokens are quite different with what the classic FCS exposes.
These tokens miss a lot of details which were otherwise provided by the FCS.
Some notable things:
- String text tokens are not marked, we need to detect the opening and closing of strings ourselves.
- The same with code comments
- Tokens in dead code are always present. Dead code being non-active code based by the compiler defines.
- Newlines can be detected in the whitespace tokens. This is a nice improvement as we can detect newlines while going over the tokens.
  Previously, we had to detect empty lines as a separate step.
- ...

### Parsing at the lower level

There are also a couple of benefits of parsing at a lower level. Some premature checks that are required for the typed tree can be skipped.
Some notable things:
- We don't support non-light syntax anymore, we no longer respect any `FSharpParsingOptions` in that regard. This will lead to a more simpler API when using Fantomas programmatically.
- Some checks about the current file being the last file in a project or a script file can be skipped as well.
  We only need to know if we are dealing with a implementation file or a signature file.
- The namespace of the Syntax tree remains the same. We don't expose everything under a Fantomas namespace.

### Collecting trivia

Our core principle of enriching the syntax tree has not change: we find things in the tokens are add them to tree.
A couple or practical things did change though:
- hash directives are captured inside the main token scanning round. Previously, we would detect those while we were searching for the different hash directives.
- Strings, byte string, interpolated string and `LibraryOnlyILAssembly` expressions are now always captured from the tokens.
  In the past, we would scan each token and decided if need to capture it as trivia based on the content.
  Once we constructed a token, we required a lookup of a trivia nodes to attach the trivia. This was a quite expensive process.
  We now, take the range of known string and capture the original text from the `ISourceText`. This is quite fast and does not require us to detect every little edges case on the token side.
  This is most welcome for interpolated strings as these can be extremely difficult to dissect.
- Empty lines between hash directives are not captured as trivia anymore.
  Something like:
  ```fsharp
  #if FOO
  someFunction a b
  printfn "meh"
  #endif
  ```
  without defines, we will capture the trivia as `#if FOO\n#endif`, were we used to capture it as `#if FOO\n\n\n#endif`.

### Finding hash defines

This is reason I started writing up this document. I admit, I got a bit carried away at the start, but this is really what I want to write down for others and my future self.

When Fantomas kicks off, it first tries to find the combination of defines that cover all code paths. Let's unpack this loaded statement with a sample:

```fsharp
#if FOO
printfn "FOO was active"
#else
printfn "no defines"
#endif
```

The parser will construct a different AST based on what compiler defines are provided.
If `FOO` is passed it will construct a tree, simplified here:
```fsharp
DoExpr
 (Yes tmp.fsx (1,0--1,24),
  App
    (NonAtomic, false, Ident printfn,
     Const
       (String ("FOO was active", Regular, tmp.fsx (1,8--1,24)),
        tmp.fsx (1,8--1,24)), tmp.fsx (1,0--1,24)),
  tmp.fsx (1,0--1,24))
```

Notice that we lost all notion of the `#if` line, no information is stored.
If we did not pass any defines, we get the counter part:
```fsharp
DoExpr
 (Yes tmp.fsx (1,0--1,24),
  App
    (NonAtomic, false, Ident printfn,
     Const
       (String ("no defines", Regular, tmp.fsx (1,8--1,24)),
        tmp.fsx (1,8--1,24)), tmp.fsx (1,0--1,24)),
  tmp.fsx (1,0--1,24))
```

To format correct output, we need to format both trees and merge them back together.
The thing is, upfront we do not know about `FOO` or any other defines.
We need to detect these in Fantomas, and here comes the important part.

Initially, we create the tokens of a tree without passing any defines.
We scan these tokens for any `HASH_IF` lines and process these if present.
Once we have all the lines, we try and detect all combinations of defines that lead to active code.
This process can be quite complex, f.ex. we can find `#if FOO && (BAR || MEH)`, nested with other fun combinations and so on.

Anyway, once we have an idea of the define combinations, we process each combo.
In the simplest scenario, no defines are present and we get the untyped ast and process the tokens to get the trivia.

In case, there are multiple combinations of defines we need to process each combination.
We go over the same tokens in each case, but we skip over the "dead code" paths.
Every time we encounter a `#if/#else/#end` line, we check with current set of defines if the next code is active or not.
If the code is active, we process the tokens the same way as if there are no defines. For non active code, we skip over most tokens.

Some examples:

```fsharp
#if FOO
0.
#else
1.
#end
```

with `[]`:
- The `#if` is detect and we solve the expression for `[]`.
  Outcome if that the next code is dead code, don't process those tokens.
  We capture the `#if` line as a trivia
- We encounter `#else`, meaning our current state gets inverted. We capture the `#else` line as trivia
  Dead becomes alive and alive becomes dead.
- `1.` is a special trivia case. Numbers cannot be trusted, therefor a trivia is created.
  We capture `#endif`, marking all the tokens that follow as active code again. We capture `#endif` as trivia.

Result:
- 3 times `Define` trivia
- 1 time `Number` trivia

with `["FOO"]`:
Some more less the same thing but inverted.

#### Inception and dead inception

> There is a dream inside a dream , I'm wide awake the more I sleep

That is more fun than one `#if ` define? A nested one of course:

```fsharp
#if FOO
    a
    #if BAR
    b
    #endif
    c
#endif
d
```

This leads to four define combinations: `[]`, `["FOO"]`, `["BAR"]`, `["FOO","BAR"]`
Depending on the combination, we will inevitably encounter a path of dead code.
We need to ignore trivia in dead code, unless the trivia are any more hash lines.

So, if we process the tokens with `[]`, we need to capture `#if BAR` as trivia even tough after the first line, we know that we are walking dead code. 
When walking the tokens with all the combinations, the only thing they will always have in common is the hash lines. These should always be there.

Since, we are messed with the more bare bone tokens, we need to detect all these things ourselves.
The solution in Fantomas (for better or worse) is `TriviaBuilderState` with `ConditionalCodeTree`.
We need to keep track in what state we are in terms of active code (and how deep down the rabbit hole we are).
And we need to know when we are inside a code comment or a string, because hash lines inside those don't count.

This code works today but if there is a more elegant way to solve this, I'm really all ears.

## Riddles in the Dark

While reading all of this, there must most certainly be questions at this point?

### Why the custom FCS layer and not just build the classic FCS from source?

Well, doing that will leave you with a local FCS nuget that depends on a local FSharp.Core nuget.
This is not ideal since we do still intend to ship a core version of Fantomas.

### What about the nightly feed of FCS?

Somewhat the same answer as the question above, the custom FSharp.Core version is a pain. 
And the nightly feed is outside of our control, this can be annoying as well. ([Example](https://github.com/dotnet/fsharp/issues/12704)).

### What happens to code generation projects?

Projects like Myriad, Snowflaqe and others would need to remove the dependency on FCS and use the syntax tree shipped with Fantomas.
This will have the same namespace as the FCS. People might need a separate build of FsAST or just include it from source.

### What about performance?

There are no real numbers available at the moment. The unit test do run faster on my local machine.
Performance was never the greatest motivator for these changes. But yes, a better syntax tree will benefit performance for sure.

## Breaking the barrel once more

It is crystal clear that all these changes should be consider breaking in semver terms.
While we have a breaking version, we can also introduce some other changes that are otherwise impossible.

### Remove Format selection

Format selection is to my knowledge only used in Rider today. And even there, it really only works when you've made a proper selection.
Chances are that this will be remove for now and added back in some more more ground work for a new implementation was done.

### Deprecate

- Remove `fsharp_semicolon_at_end_of_line`, both style guides do not have it.
- Remove `fsharp_keep_indent_in_branch`, does not do anything anymore.
- Remove `fsharp_indent_on_try_with`, both style guides do not have it.
- cli:
  - Remove `--stdin`
  - Remove `--stdout`
  - Remove `--fsi`
  - Check what `--force` should do?

### Fantomas.Extras

The original idea of Fantomas.Extras was the usage within FAKE.
Nowadays, we recommend using the CLI tool in FAKE.

- Remove Fantomas.Extras.
- Extract the editorconfig function to `Fantomas.Editorconfig`.
- Consider `Fantomas.FAKE`, wrapping the CLI tool as FAKE targets.

### Elmish  code

We currently support [Elmish](./Formatting-Elmish-code.md) syntax by default. This was a mistake and we should combine `fsharp_single_argument_web_mode` and `fsharp_disable_elmish_syntax` into one setting.
This setting `elmish_mode` would have three accepted values: `none`, `single` and `dual`.

### Fantomas.Core

The Fantomas library is now carrying the Fantomas name on NuGet. This is confusing when installing the dotnet tool.
We should rename `Fantomas` to `Fantomas.Core` and `Fantomas.GlobalTool` to `Fantomas` (so we can have `dotnet tool install fantomas`).
Daemon mode should keep working in this case.


## Return of the FCS or the Last Fantomas?

At some point in the future, the F# compiler will contain all the Fantomas related updates and there will be no point anymore in using our own FCS.
This because, we would have no reason anymore to move our own git commit pointer forward.
If that day ever comes, we could revert our custom FCS experiment and more back to using the classic FCS on nuget.

However, in the utopian scenario above, we could also hope that Fantomas will be absorb by the FCS at that point.
And it is part of the exposed API, ever present in the vast F# ecosystem.

Perhaps, it will be mix of both. Fantomas keeps moving forward with the latest and greatest of the compiler.
While the FCS moves at a slower speed according to its own release cycle.
Who knows am I right?

## Closing thoughts

Fantomas Five is both exciting and scary at the same time.
I believe it is worth doing this, to improve the syntax tree and immediately have those gains.
The important part is to take the time required to figure everything out and verify this idea will work out.

Thanks for reading all of this, take care!

Nojaf

## Appendix

I feel the need to explain the "Bare Metal Alchemist" subtitle.
"Bare Metal" because we are closer than ever to the F# compiler.
"Bare Metal Alchemist" is also a play on "Full Metal Alchemist", a manga and anime series which tells the tale about two brothers that messed with forces they shouldn't have.
This feels very fitting as I'm not a 100% sure, that creating our own FCS is something that we should be doing.
Oh and enjoy this [one](https://youtu.be/ASWcIZWmXfk), it is the song in the anime where the two brothers burnt down their own house to signify that there is no way back.
Which is again somewhat symbolic, once we released Fantomas 5, there is no way back without another major version, something I wish to avoid at all times.