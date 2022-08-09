Fantomas
========

[![Build Status Github Actions](https://github.com/fsprojects/fantomas/workflows/Build%20master/badge.svg?branch=master&event=push)](https://github.com/fsprojects/fantomas/actions)
[![Discord](https://img.shields.io/discord/940511234179096586?label=Fantomas%20Discord&style=flat-square)](https://discord.gg/D5QXvQrBVa)

F# source code formatter, inspired by [scalariform](https://github.com/mdr/scalariform) for Scala, [ocp-indent](https://github.com/OCamlPro/ocp-indent) for OCaml and [PythonTidy](https://github.com/acdha/PythonTidy) for Python.

## Purpose
This project aims at formatting F# source files based on a given configuration.
Fantomas will ensure correct indentation and consistent spacing between elements in the source files.
We assume that the source files are *parsable by F# compiler* before feeding into the tool.
Fantomas follows two F# style guides: the [F# code formatting guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting) from Microsoft by default and the [G-Research F# code formatting guidelines](https://github.com/G-Research/fsharp-formatting-conventions) via various [settings](https://github.com/G-Research/fsharp-formatting-conventions/blob/master/.editorconfig).

## Contributing Guidelines

See the [Contribution Guidelines](./CONTRIBUTING.md).

## Credits
We would like to gratefully thank the following persons for their contributions.
https://github.com/fsprojects/fantomas/graphs/contributors

## License
The library and tool are available under Apache 2.0 license.
For more information see the [License file](LICENSE.md).

<div class="text-end my-4">
    <a href="./end-users/GettingStarted.html">Next</a>
</div>