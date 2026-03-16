# History

This page provides architectural history and context for key decisions in the Fantomas project.
Understanding **why** things are the way they are can help new contributors navigate the codebase.

## The FCS coupling problem

Fantomas relies on the F# compiler's parser to construct an [Untyped Abstract Syntax Tree](https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-syntaxtree.html) (AST).
Historically, Fantomas consumed the parser through the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) (FCS) NuGet package.

This created a painful coupling. The FCS release cycle was tied to .NET SDK releases, so it could take months before a PR that improved the syntax tree could be utilized in Fantomas.
To improve the core of Fantomas, we often needed to submit PRs to the [F# compiler](https://github.com/dotnet/fsharp) itself. These improvements would then sit unreleased for a long time, blocking progress.

## Decoupling from editors (v4.6)

The 4.6 release introduced **daemon mode**, which decoupled Fantomas from the editors.
Instead of editors consuming `Fantomas.Core` as a DLL (which forced alignment of FCS versions between Fantomas and editor tooling), editors now interact with the `fantomas` command line tool via the `Fantomas.Client` library.

This meant end-users could bring their own version of Fantomas. Editors no longer needed to bundle a specific FCS version, which freed Fantomas from having to use the public FCS packages on NuGet.

## Creating Fantomas.FCS (v5)

With the editor coupling removed, Fantomas 5 took the next step: creating a custom, lightweight fork of the F# compiler parser.

### How it works

`Fantomas.FCS` takes only the source files necessary to compile the F# lexer and parser from the [dotnet/fsharp](https://github.com/dotnet/fsharp) repository, at a specific git commit. This is done via custom code in a `Fun.Build` pipeline.

Key properties of this approach:

* We only expose the lexer and parser (early compiler phases), significantly reducing the dependency footprint compared to the full FCS NuGet package.

* We can move forward as soon as a relevant PR is merged to the `dotnet/fsharp` main branch, without waiting for an official NuGet release.

* The AST returned by `Fantomas.FCS` looks identical to what the official F# compiler returns, but is not binary compatible. Fantomas typically contains a newer version of the syntax tree than the official compiler.

### Why not just build the classic FCS from source?

Building the full FCS from source produces a local FCS NuGet that depends on a local FSharp.Core NuGet. This is not ideal for shipping `Fantomas.Core` as a library.

### Why not use the nightly FCS feed?

The nightly feed is outside of our control and has caused problems in the past. The custom FSharp.Core version dependency remains an issue there as well.

## Future outlook

At some point, the F# compiler may contain all the Fantomas-related improvements, making the custom FCS unnecessary. If that day comes, we could revert to using the official FCS NuGet package.

Another possibility is that Fantomas could eventually be absorbed into the FCS itself, becoming part of the exposed API in the F# ecosystem. In practice, Fantomas will likely continue moving forward with the latest compiler changes, while the official FCS moves at its own release cadence.

<fantomas-nav previous="Formatting%20Conventions.md" next="Glossary.md"></fantomas-nav>