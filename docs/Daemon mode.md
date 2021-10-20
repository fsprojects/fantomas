# Fantomas Daemon mode

## Introduction

As part of the `4.6` release, we've introduced a new feature where end-user can control the version of Fantomas that is being used inside an IDE.
In previous iterations, the editor tooling would typically reference the [Fantomas](https://www.nuget.org/packages/Fantomas) or [Fantomas.Extras](https://www.nuget.org/packages/Fantomas.Extras) nuget package and use the [CodeFormatter](../src/Fantomas/CodeFormatter.fsi) api to handle formatting.
The major drawback of this approach is that shared [FCS](https://www.nuget.org/packages/FSharp.Compiler.Service/) dependency needed to be exactly the same.
So the editor is in control of which version of Fantomas is being used.
Each version of Fantomas theoretically can have a different outcome as the style guides may have changed over time.

## Solution

To tackle this problem, we introduce two new concepts: `--daemon` mode for the [fantomas-tool](https://www.nuget.org/packages/fantomas-tool) and [Fantomas.Client](https://www.nuget.org/packages/Fantomas.Client).
`--daemon` would launch the commandline application as a sort of [LSP server](https://microsoft.github.io/language-server-protocol/) and `Fantomas.Client` could connect to this and proxy format requests.
Editor tooling would be able to launch your pinned version of `fantomas-tool` as a daemon service and interact with it outside-of-process.

## End-user impact

End-users don't have to worry about `Fantomas.Client` or the `--daemon` flag. They only need to install a compatible version of `fantomas-tool`.
Be it locally or globally. The first compatible version is `4.6.0-alpha-004`, all higher version should work as well.
Local versions have precedence over the global version.

The nice thing about this approach is that you can upgrade Fantomas at your own pace. 
When new versions drop, you can dedicate a separate commit in source control and it won't interfere with your other commits.