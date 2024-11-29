---
category: End-users
categoryindex: 1
index: 11
---
# Upgrade guide

We wish to capture all changes required to upgrade to a new version. Please note that the focus of this document is about how to upgrade.  
New features are not covered in detail here, for those please refer to our [changelog](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md).  
If you find something to be missing from this guide, please consider opening a PR to mend the gap instead of opening an issue.



## v5.0

### .editorconfig

- `fsharp_max_elmish_width` was removed.
- `fsharp_single_argument_web_mode` was removed.
- `fsharp_disable_elmish_syntax` was removed.
- `fsharp_semicolon_at_end_of_line` was removed.
- `fsharp_keep_if_then_in_same_line` was removed.
- `fsharp_indent_on_try_with` was removed.
- If you were using Elmish inspired code (or `fsharp_single_argument_web_mode`) use

```
fsharp_multiline_block_brackets_on_same_column = true
fsharp_experimental_stroustrup_style = true
```
- `fsharp_keep_indent_in_branch ` was renamed to `fsharp_experimental_keep_indent_in_branch`

### console application

- The dotnet tool is now targeting `net6.0`.
- `--stdin` was removed.
- `--stdout` was removed.
- `--fsi` was removed.
- `--force` now writes a formatted file to disk, regardless of its validity.

### Miscellaneous

- NuGet package `Fantomas` was renamed to `Fantomas.Core`.
- NuGet package `fantomas-tool` was renamed to `fantomas`.
- `Fantomas.Core` uses [Fantomas.FCS](https://www.nuget.org/packages/Fantomas.FCS) instead of [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)
- NuGet package `Fantomas.Extras` is deprecated.

## v5.1

### .editorconfig

- The space in patterns is no longer controlled by `fsharp_space_before_parameter`,  
  `fsharp_space_before_lowercase_invocation` and `fsharp_space_before_uppercase_invocation` are now used.

## v5.2

### .editorconfig

- `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style` are now merged into one setting `fsharp_multiline_bracket_style`.  
  The accepted values for `fsharp_multiline_bracket_style` are `cramped`, `aligned` and `experimental_stroustrup`.  <br />
  Note that `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style` will continue to work until the next major version.

## v6.0

### .editorconfig

- `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style` are replaced with `fsharp_multiline_bracket_style`
- `experimental_stroustrup` for `fsharp_multiline_bracket_style` is now `stroustrup`
- `fsharp_newline_before_multiline_computation_expression` was extracted from `fsharp_multiline_bracket_style = stroustrup` and now controls how computation expression behave.
- `fsharp_strict_mode` was removed and can no longer be used.

### console application
- `-v` is now short for `--verbosity` instead of `--version`
- The console output was revamped.
- `--recurse` was removed. Please use [.fantomasignore](./IgnoreFiles.html) file if you wish to ignore certain files.

### Miscellaneous
- The public API of CodeFormatter no longer uses `FSharpOption<'T>`, instead overloads are now used.
- `StrictMode` was removed from `FormatConfig`, not passing the source text in the public API will have the same effect.

## v6.1

### Miscellaneous
- The namespace in [Fantomas.FCS](https://www.nuget.org/packages/Fantomas.FCS) changed from `FSharp.Compiler` to `Fantomas.FCS`.

## v7 alpha

### console application
- Target framework is now `net8.0`.

### .editorconfig
- `fsharp_max_dot_get_expression_width` was removed.

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
