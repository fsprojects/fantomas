## Fantomas: How to use

### Using the command line tool

---

For the overview how to use the tool, you can type the command

	Fantomas --help

You have to specify an input path and optionally an output path. 
The output path is prompted by `--out` e.g.

	Fantomas ../../../../tests/stackexchange/array.fs --out ../../../../tests/stackexchange_output/array.fs 

Both paths have to be files or folders at the same time. 
If they are folders, the structure of input folder will be reflected in the output one. 
The tool will explore the input folder recursively if you set `--recurse` option (see [Options section](#options)).
If you omit the output path, Fantomas will overwrite the input files.

#### Options

##### `--recurse`

traverse the input folder recursively (if it is really a folder) to get all F# source files.

##### `--force`

force writing original contents to output files. 
This is helpful if the tool fails on some unknown F# constructs.

##### `--stdin`

read input from standard input. This option is convenient to use with piping

    echo 'open System;; let () = printfn "Hello World"' | Fantomas --stdin --out output.fs

or

    cat input.fs | Fantomas --stdin --out output.fs

##### `--stdout`

write formatted source code to standard output e.g.
 
    Fantomas input.fs --stdout

##### `--fsi`

this option to be used with `--stdin` to specify that we are formatting F# signatures e.g.

    type input.fsi | Fantomas --fsi --stdin --stdout

##### `--check`

Checks if the files provided require formatting and:

* Exits with `0` if no files require formatting
* Exits with `1` if some files require formatting. It also outputs the path of the files that require formatting.
* Exits with `99` if some files contain errors (e.g. parsing errors, etc.)

For example:

	# given an example project
	ls src/MyProject
	File1.fs # correctly formatted
	File2.fs # needs formatting
	File3.fs # has compilation errors

	# running a check
	Fantomas --check src/MyProject
	src/MyProject/File2.fs requires formatting
	error: Failed to format src/MyProject/File3.fs: <description of the error>

	# if you check the exit code
	echo $?
	99

#### Preferences

##### `--indent <number>`

`number` has to be between 1 and 10.

This preference sets the indentation (default = 4). 
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file. 
To illustrate, here is a code fragment with `--indent 2`:

	```fsharp
	let inline selectRandom(f : _[]) = 
	  let r = random 1.0
	  let rec find = 
	    function 
	    | 0 -> fst f.[0]
	    | n when r < snd f.[n] -> fst f.[n]
	    | n -> find(n - 1)
	  find <| f.Length - 1
	```

##### `--pageWidth <number>`

`number` has to be an integer greater or equal to 60.
This preference sets the column where we break F# constructs into new lines.
The default value is 120. To see its effects, please take a look at some [output files](tests/stackexchange_output) with `--pageWidth 90` preference.

##### `--semicolonEOL`

add semicolons at the end of lines e.g.

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
	
	vs.
	
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

##### `--spaceBeforeColon`

if being set, there is a space before `:` e.g.

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
