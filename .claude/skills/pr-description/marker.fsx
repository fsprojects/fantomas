// Loaded by build.fsx and compare.fsx: how a built tree records which project was built in it, and
// with which recipe.
open System.IO
open System.Text.RegularExpressions

/// What a build is made with. A tree built with an older recipe counts as not built.
let recipe: string = "source-revision"

let path (tree: string) (project: string) : string =
    Path.Combine(tree, ".built-" + Regex.Replace(project, "[^A-Za-z0-9.]", "_"))

let isBuilt (tree: string) (project: string) : bool =
    let marker: string = path tree project
    File.Exists marker && File.ReadAllText marker = recipe
