---
category: Contributors
categoryindex: 2
index: 13
---
# Updating the compiler sources

From time to time we want to update the sources of the F# compiler we use for our own parser (Fantomas.FCS).  
Reasons can be bugfixes or new features we want to use.
Examples are range fixes or newly added information in the AST we want to make use of.

To do this, first remove the old compiler sources by running:
```shell
git clean -xdf
```

Make sure, that this removes your `.deps` folder.  
Next update the hash of the source version to use. Edit the `FCSCommitHash` value in the `Directory.Build.props` file.
Run 
```shell
dotnet fsi .\build.fsx -p Init
```
to download the new sources into the `.deps` folder. Make sure there's one directory in `.deps` named like the configured hash afterwards.

You can now run a build to see if there is any obvious breakage:
```shell
dotnet build
```

If not, you can run the `Fantomas.Core` tests next
```shell
cd ./src/Fantomas.Core.Tests
dotnet test
```

Even if the tests are all green you should take a look at all the changes made to the [SyntaxTree](https://github.com/dotnet/fsharp/commits/main/src/Compiler/SyntaxTree) and make sure these changes don't need further adjustments in Fantomas.  

Think about tests to catch any regressions caused by the update and it's effects on Fantomas.

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>