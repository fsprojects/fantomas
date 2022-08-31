Fantomas
========

![Fantomas logo](https://raw.githubusercontent.com/fsprojects/fantomas/master/fantomas_logo.png)

F# source code formatter, inspired by [scalariform](https://github.com/mdr/scalariform) for Scala, [ocp-indent](https://github.com/OCamlPro/ocp-indent) for OCaml and [PythonTidy](https://github.com/acdha/PythonTidy) for Python.

![GitHub Workflow Status (event)](https://img.shields.io/github/workflow/status/fsprojects/fantomas/Build%20master?event=push&label=Build%20master&style=flat-square)
[![Discord](https://img.shields.io/discord/940511234179096586?label=Fantomas%20Discord&style=flat-square)](https://discord.gg/D5QXvQrBVa)
[![Nuget (with prereleases)](https://img.shields.io/nuget/vpre/fantomas?style=flat-square)](https://www.nuget.org/packages/fantomas/absoluteLatest)
## Early builds

Every once in a while an `alpha` or `beta` version is published to https://www.nuget.org/.
Our previous MyGet feed is now deprecated.

## Purpose
This project aims at formatting F# source files based on a given configuration.
Fantomas will ensure correct indentation and consistent spacing between elements in the source files.
We assume that the source files are *parsable by F# compiler* before feeding into the tool.
Fantomas follows two F# style guides: the [F# code formatting guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting) from Microsoft by default and the [G-Research F# code formatting guidelines](https://github.com/G-Research/fsharp-formatting-conventions) via various [settings](https://github.com/G-Research/fsharp-formatting-conventions/blob/master/.editorconfig).

## Contributing Guidelines

See the [Contribution Guidelines](./CONTRIBUTING.md).

## Credits
We would like to gratefully thank the following persons for their contributions.
 - [Eric Taucher](https://github.com/EricGT)
 - [Steffen Forkmann](https://github.com/forki)
 - [Jack Pappas](https://github.com/jack-pappas)
 - [Ivan Towlson](https://github.com/itowlson)
 - [Don Syme](https://github.com/dsyme)
 - [Gustavo Guerra](https://github.com/ovatsus)
 - [Jared Parsons](https://github.com/jaredpar)
 - [Denis Ok](https://github.com/OkayX6)
 - [Enrico Sada](https://github.com/enricosada)

## License
The library and tool are available under Apache 2.0 license.
For more information see the [License file](LICENSE.md).
