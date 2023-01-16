type Command = { File: string; Arguments: string }

let dotnet =
    {|
        restore =
            {
                File = "dotnet"
                Arguments = "restore"
            }
        build = { File = "dotnet"; Arguments = "build" }
        tool =
            {|
                restore =
                    {
                        File = "dotnet"
                        Arguments = "tool restore"
                    }
            |}
        paket =
            {|
                restore =
                    {
                        File = "dotnet"
                        Arguments = "paket restore"
                    }
            |}
    |}

type Repository =
    {
        Name: string
        GitUrl: string
        Commit: string
        Prepare: Command list
        Build: Command list
        Format: string list
    }

let repositories =
    [
        {
            Name = "fantomas"
            GitUrl = "https://github.com/fsprojects/fantomas"
            Commit = "3dc34284a4f385f2773fedcc91cd8b9736a4d970"
            Prepare = [ dotnet.restore ]
            Build = [ dotnet.build ]
            Format = [ "src"; "build.fsx"; "docs" ]
        }
        {
            Name = "fsautocomplete"
            GitUrl = "https://github.com/fsharp/fsautocomplete"
            Commit = "d74332594a29b8bae412032a2e75a4818f7a633f"
            Prepare =
                [
                    dotnet.tool.restore
                    dotnet.restore
                    {
                        File = "dotnet"
                        Arguments = "run --project build -t ReplaceFsLibLogNamespaces"
                    }
                ]
            Build = [ dotnet.build ]
            Format = [ "src" ]
        }
        {
            Name = "fsharp"
            GitUrl = "https://github.com/dotnet/fsharp"
            Commit = "0677fc2f7d0612388351d035480005af4d90ad49"
            Prepare = []
            Build =
                [
                    {
                        File = "Build.cmd"
                        Arguments = "-noVisualStudio"
                    }
                ]
            Format = [ "src" ]
        }
        {
            Name = "FSharp.Formatting"
            GitUrl = "https://github.com/fsprojects/FSharp.Formatting"
            Commit = "bd0ba65c5dcc8cfc081562435c507a5cfb84cb88"
            Prepare =
                [
                    dotnet.tool.restore
                    dotnet.restore
                    dotnet.paket.restore
                    {
                        File = "dotnet"
                        Arguments = "run --project ./build/build.fsproj -t AssemblyInfo"
                    }
                ]
            Build = [ dotnet.build ]
            Format = [ "src"; "tests"; "docs"; "build" ]
        }
        {
            Name = "plotly"
            GitUrl = "https://github.com/plotly/Plotly.NET"
            Commit = "97a6b687fca0ebcba73c3ffb0d19788e6a668376"
            Prepare = [ dotnet.tool.restore; dotnet.restore ]
            Build = [ dotnet.build ]
            Format = [ "src" ]
        }
        {
            Name = "ApiSurface"
            GitUrl = "https://github.com/G-Research/ApiSurface"
            Commit = "49520f35806e38492da75501063dae90687e10c7"
            Prepare = [ dotnet.tool.restore; dotnet.restore ]
            Build = [ dotnet.build ]
            Format = [ "." ]
        }
        {
            Name = "farmer"
            GitUrl = "https://github.com/CompositionalIT/farmer"
            Commit = "4702b7db2ae661706ecda3bad83206897e0a3f93"
            Prepare = [ dotnet.restore ]
            Build = [ dotnet.build ]
            Format = [ "src"; "samples" ]
        }
        {
            Name = "IcedTasks"
            GitUrl = "https://github.com/TheAngryByrd/IcedTasks"
            Commit = "8ee97f9c0b6c7cedbde3edd3ae6ad96f7d68dac6"
            Prepare = [ dotnet.tool.restore; dotnet.paket.restore ]
            Build = [ dotnet.build ]
            Format = [ "src" ]
        }
        {
            Name = "FSharp.Data"
            GitUrl = "https://github.com/fsprojects/FSharp.Data"
            Commit = "dae308988c1ae43368508ec65e2ce4c58b59406a"
            Prepare = [ dotnet.tool.restore; dotnet.paket.restore ]
            Build = [ dotnet.build ]
            Format = [ "src"; "build.fsx" ]
        }
    ]
