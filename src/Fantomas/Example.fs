module Example
// Open the namespace with InteractiveChecker type
open System
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance (ignore notifications)
let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty ignore)

// Sample input for the compiler service
let input = """
  module Tests
  let foo() = 
    let msg = "Hello world"
    printfn "%s" msg """
let file = "/home/user/Test.fs"

// Get compiler options for a single script file
let checkOptions = checker.GetCheckOptionsFromScriptRoot(file, input, DateTime.Now, [| |])
// Run the first phase (untyped parsing) of the compiler
let untypedRes = checker.UntypedParse(file, input, checkOptions)
match untypedRes.ParseTree with
| Some tree ->  printfn "Got tree %A" tree
| None -> printfn "Nothing :("
