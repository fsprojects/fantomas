---
category: End-user documentation
categoryindex: 1
index: 3
---
# JetBrains Rider

The [resharper-fsharp](https://github.com/JetBrains/resharper-fsharp) uses fantomas under the hood to format the source code. No need for any additional plugins.

#### Using the latest version inside Rider

For technical reasons Rider cannot always use the latest version of Fantomas found on NuGet.
As a workaround you could install [fantomas](https://www.nuget.org/packages/fantomas-tool) locally with `dotnet tool install fantomas-tool` and configure it as an [External tool](https://www.jetbrains.com/help/rider/Settings_Tools_External_Tools.html).

![Rider external tool window](./docs/rider-external-tool.png)

![Rider action window](./docs/rider-action-window.png)

**This will have an impact on your editing experiencing in Rider**, the external change to the file by the command line application might trigger more internal logic inside Rider than necessary.
It could be noticeable in regards to the default formatting experience.
