# Fantomas: How to use

## Using the command line tool

---

Install the command line tool with:
> dotnet tool install fantomas-tool

For the overview how to use the tool, you can type the command

	dotnet fantomas --help

```
USAGE: dotnet fantomas [--help] [--recurse] [--force] [--profile] [--fsi <string>] [--stdin] [--stdout] [--out <string>] [--check] [--version] [<string>]

INPUT:

    <string>              Input path: can be a folder or file with *.fs,*.fsi,*.fsx,*.ml,*.mli extension.

OPTIONS:

    --recurse, -r         Process the input folder recursively.
    --force               Print the source unchanged if it cannot be parsed correctly.
    --profile             Print performance profiling information.
    --fsi <string>        Read F# source from stdin as F# signatures.
    --stdin               Read F# source from standard input.
    --stdout               Write the formatted source code to standard output.
    --out <string>        Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only.
    --check               Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error
                          was an internal error
    --version, -v         Displays the version of Fantomas
    --help                display this list of options.

```

You have to specify an input path and optionally an output path. 
The output path is prompted by `--out` e.g.

	dotnet fantomas ../../../../tests/stackexchange/array.fs --out ../../../../tests/stackexchange_output/array.fs 

Both paths have to be files or folders at the same time. 
If they are folders, the structure of input folder will be reflected in the output one. 
The tool will explore the input folder recursively if you set `--recurse` option (see [Options section](#options)).
If you omit the output path, Fantomas will overwrite the input files.

## Configuration

Fantomas ships with a series of format options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the commandline tool.

A default .editorconfig file would look like
```ini
[*.fs]
indent_size=4
max_line_length=120
fsharp_semicolon_at_end_of_line=false
fsharp_space_before_parameter=true
fsharp_space_before_lowercase_invocation=true
fsharp_space_before_uppercase_invocation=false
fsharp_space_before_class_constructor=false
fsharp_space_before_member=false
fsharp_space_before_colon=false
fsharp_space_after_comma=true
fsharp_space_before_semicolon=false
fsharp_space_after_semicolon=true
fsharp_indent_on_try_with=false
fsharp_space_around_delimiter=true
fsharp_max_if_then_else_short_width=40
fsharp_max_infix_operator_expression=50
fsharp_max_record_width=40
fsharp_max_array_or_list_width=40
fsharp_max_value_binding_width=40
fsharp_max_function_binding_width=40
fsharp_multiline_block_brackets_on_same_column=false
fsharp_newline_between_type_definition_and_members=false
fsharp_keep_if_then_in_same_line=false
fsharp_max_elmish_width=40
fsharp_single_argument_web_mode=false
fsharp_strict_mode=false
```

### indent_size

` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`

```fsharp
let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1
```

`{ defaultConfig with IdentSize = 2 }`

```fsharp
let inline selectRandom (f: _ []) =
  let r = random 1.0

  let rec find =
    function
    | 0 -> fst f.[0]
    | n when r < snd f.[n] -> fst f.[n]
    | n -> find (n - 1)

  find <| f.Length - 1
```

### max_line_length

`max_line_length` has to be an integer greater or equal to 60.
This preference sets the column where we break F# constructs into new lines.
Default = 120.

`defaultConfig`

```fsharp
match myValue with
| Some foo -> someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"
```

`{ defaultConfig with MaxLineLength = 60 }`

```fsharp
match myValue with
| Some foo ->
    someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"
```

### fsharp_semicolon_at_end_of_line

Add semicolons at the end of lines.
Default = false.

`defaultConfig`

```fsharp
let saturn =
    { X = 8.343366718
      Y = 4.124798564
      Z = -0.4035234171
      VX = -0.002767425107 * daysPerYear
      VY = 0.004998528012 * daysPerYear
      VZ = 2.304172976e-05 * daysPerYear
      Mass = 0.0002858859807 * solarMass }
```

`{ defaultConfig with SemicolonAtEndOfLine = true }`

```fsharp
let saturn =
    { X = 8.343366718;
      Y = 4.124798564;
      Z = -0.4035234171;
      VX = -0.002767425107 * daysPerYear;
      VY = 0.004998528012 * daysPerYear;
      VZ = 2.304172976e-05 * daysPerYear;
      Mass = 0.0002858859807 * solarMass }
```

### fsharp_space_before_parameter

Add a space after the name of a function and before the opening parenthesis of the first parameter.
This setting influences function definitions.
Default = true.

`defaultConfig`

```fsharp
let value (a:int) = x
let DumpTrace () = ()
```

`{ defaultConfig with SpaceBeforeParameter = false }`

```fsharp
let value(a: int) = x
let DumpTrace() = ()
```

### fsharp_space_before_lowercase_invocation

Add a space after the name of a lowercased function and before the opening parenthesis of the first argument.
This setting influences function invocation.
Default = true.

`defaultConfig`

```fsharp
value (a, b)
startTimer ()
```

`{ defaultConfig with SpaceBeforeLowercaseInvocation = false }`

```fsharp
value(a, b)
startTimer()
```

### fsharp_space_before_uppercase_invocation

Add a space after the name of a uppercased function and before the opening parenthesis of the first argument.
This setting influences function invocation.
Default = false.

`defaultConfig`

```fsharp
Value(a, b)
person.ToString()
```

`{ defaultConfig with SpaceBeforeUppercaseInvocation = true }`

```fsharp
Value (a, b)
person.ToString ()
```

### fsharp_space_before_class_constructor

Add a space after a type name and before the class constructor.
Default = false.

`defaultConfig`

```fsharp
type Person() =
    class
    end
```

`{ defaultConfig with SpaceBeforeClassConstructor = true }

```fsharp
type Person () =
    class
    end
```

### fsharp_space_before_member

Add a space after a member name and before the opening parenthesis of the first parameter.
Default = false.

`defaultConfig`

```fsharp
type Person() =
    member this.Walk(distance: int) = ()
    member this.Sleep() = ignore
    member __.singAlong() = ()
    member __.swim(duration: TimeSpan) = ()
```

`{ defaultConfig with SpaceBeforeMember = true }`

```fsharp
type Person() =
    member this.Walk (distance: int) = ()
    member this.Sleep () = ignore
    member __.singAlong () = ()
    member __.swim (duration: TimeSpan) = ()
```

### fsharp_space_before_colon

Add a space before `:`.
Default = false.

`defaultConfig`

```fsharp
type Point = { x: int; y: int }

let update (msg: Msg) (model: Model): Model = model
```

`{ defaultConfig with SpaceBeforeColon = true }`

```fsharp
type Point = { x : int; y : int }

let update (msg : Msg) (model : Model) : Model = model
```

	```fsharp
	type Planet = 
	  { mutable X: float
	    mutable Y: float
	    mutable Z: float
	    mutable VX: float
	    mutable VY: float
	    mutable VZ: float
	    Mass: float }
	```
	
	vs.
	
	```fsharp
	type Planet = 
	  { mutable X : float
	    mutable Y : float
	    mutable Z : float
	    mutable VX : float
	    mutable VY : float
	    mutable VZ : float
	    Mass : float }
	```

##### `--noSpaceAfterComma`

is useful if you would like to save spaces in tuples, arguments, etc. 
To illustrate, `(1, 2, 3)` is rewritten to `(1,2,3)`.

##### `--noSpaceAfterSemiColon`

saves spaces on records, arrays, lists, etc. Now 

	```fsharp
	let planets = [|sun; jupiter; saturn; uranus; neptune|]
	```

	becomes

	```fsharp
	let planets = [|sun;jupiter;saturn;uranus;neptune|]
	```

Note: there is also the possibility to add a space before the semicolon.<br />
This can be set by adding `"SpaceBeforeSemicolon": true` in a Fantomas configuration file (see below).

 ##### `--indentOnTryWith`

if being set, `with` blocks will be indented like in the following example:

	```fsharp
	try
	    if System.DateTime.Now.Second % 3 = 0 
		then raise(new System.Exception())
	    else raise(new System.ApplicationException())
	with
	    | :? System.ApplicationException -> 
	        printfn "A second that was not a multiple of 3"    
	    | _ -> 
	        printfn "A second that was a multiple of 3"
	```

##### `--noSpaceAroundDelimiter`

saves spaces around delimiters of records, arrays, lists e.g.

    ```fsharp
	let planets = [| sun; jupiter; saturn; uranus; neptune |]
	```

	becomes

	```fsharp
	let planets = [|sun; jupiter; saturn; uranus; neptune|]
	```

##### `--strictMode`

if being set, pretty printing is only done via ASTs. Compiler directives, inline comments and block comments will be ignored. 

##### `--maxIfThenElseShortWidth  <number>`

`number` if being set, controls when if/then/else expressions will be formatted as single line or as multiple lines.

Fantomas tries to follow [the F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions) as close as possible when formatting if expressions.

The style guide says:

> If either cond, e1 or e2 are longer, but not multi-line:

```fsharp
if cond
then e1
else e2
```

But what exactly is longer right? By default Fantomas will use 40 width to determine if the expression needs to be formatted to the example above or remain as a oneliner (`if cond then e1 else e2`).


So if either `cond`, `e1` or `e2` is longer than `maxIfThenElseShortWidth` but not multiline, it will format on three lines.
See [unit tests](https://github.com/fsprojects/fantomas/blob/9d4b499c09a1f06f5485835817844657cc51215b/src/Fantomas.Tests/IfThenElseTests.fs#L734) for more examples.

That said, most of the preferences are very simple. 
But they demonstrate the flexibility of Fantomas on a set of configurations. 
More preferences will be added depending on use cases.

##### Settings configuration

Use an .editorconfig configuration file to set the formatting options.

A default .editorconfig file would look like
```ini
[*.fs]
indent_size=4
max_line_length=120
fsharp_semicolon_at_end_of_line=false
fsharp_space_before_parameter=true
fsharp_space_before_lowercase_invocation=true
fsharp_space_before_uppercase_invocation=false
fsharp_space_before_class_constructor=false
fsharp_space_before_member=false
fsharp_space_before_colon=false
fsharp_space_after_comma=true
fsharp_space_before_semicolon=false
fsharp_space_after_semicolon=true
fsharp_indent_on_try_with=false
fsharp_space_around_delimiter=true
fsharp_max_if_then_else_short_width=40
fsharp_max_infix_operator_expression=50
fsharp_max_record_width=40
fsharp_max_array_or_list_width=40
fsharp_max_value_binding_width=40
fsharp_max_function_binding_width=40
fsharp_multiline_block_brackets_on_same_column=false
fsharp_newline_between_type_definition_and_members=false
fsharp_keep_if_then_in_same_line=false
fsharp_max_elmish_width=40
fsharp_single_argument_web_mode=false
fsharp_strict_mode=false
```

### Using the API

See [CodeFormatter.fsi](../src/Fantomas/CodeFormatter.fsi) to view the API of Fantomas.
