// This main module is only necessary if you want to debug tests in VS Code. 
// dotnet test should still work as usual
//
// For example you could add the following config to your launch.json file (assuming you have the C# extension installed):
//
//  {
//     "name": "Launch CoreGlobalTool.Tests",
//     "type": "coreclr",
//     "request": "launch",
//     "preLaunchTask": "build",
//     "program": "${workspaceFolder}/src/Fantomas.CoreGlobalTool.Tests/bin/Debug/netcoreapp3.1/Fantomas.CoreGlobalTool.Tests.dll",
//     "args": [],
//     "cwd": "${workspaceFolder}",
//     "console": "internalConsole",
//     "stopAtEntry": false
// }
//
// You then assign various break points on this project and they should fire just fine.
// If you want to run a specific test or namespace, use nunit's test selection language, by modifying the args attribute:
//
// Run only the "Files with errors should report an internal error test":
//
// "args": ["--where","name == \"Files with errors should report an internal error\""],
//
// More info on the selection language can be found here: https://github.com/nunit/docs/wiki/Test-Selection-Language
module Fantomas.CoreGlobalTool.Tests.Program

open NUnitLite

[<EntryPointAttribute>]
let main argv =
    let runner = AutoRun()
    runner.Execute(argv)
