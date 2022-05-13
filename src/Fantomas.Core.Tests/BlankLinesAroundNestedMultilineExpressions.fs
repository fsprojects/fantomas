module Fantomas.Core.Tests.BlankLinesAroundNestedMultilineExpressions

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config = { config with BlankLinesAroundNestedMultilineExpressions = false }

[<Test>]
let ``basic behavior`` () =
    formatSourceString
        false
        """
let topLevelFunction () =
    let innerValue = 23
    let innerMultilineFunction () =
        // some comment
        printfn "foo"
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let topLevelFunction () =
    let innerValue = 23
    let innerMultilineFunction () =
        // some comment
        printfn "foo"
    ()
"""

[<Test>]
let ``existing newlines are preserved`` () =
    formatSourceString
        false
        """
let topLevelFunction () =
    let innerValue = 23

    let innerMultilineFunction () =
        // some comment
        printfn "foo"
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let topLevelFunction () =
    let innerValue = 23

    let innerMultilineFunction () =
        // some comment
        printfn "foo"
    ()
"""

[<Test>]
let ``with sequential expressions`` () =
    formatSourceString
        false
        """
let topLevelFunction () =
    printfn "Something to print"
    try
            nothing ()
    with
    | ex ->
        splash ()
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let topLevelFunction () =
    printfn "Something to print"
    try
        nothing ()
    with
    | ex -> splash ()
    ()
"""

[<Test>]
let ``disable blank lines around multiline expressions inside let binding, 1370`` () =
    formatSourceString
        false
        """
let emit nodes =
    let an = AssemblyName("a")
    let ab =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            an,
            AssemblyBuilderAccess.RunAndCollect
        )
    let mb = ab.DefineDynamicModule("a")
    let tb = mb.DefineType("Program")
    nodes
"""
        config
    |> prepend newline
    |> should
        equal
        """
let emit nodes =
    let an = AssemblyName("a")
    let ab =
        AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.RunAndCollect)
    let mb = ab.DefineDynamicModule("a")
    let tb = mb.DefineType("Program")
    nodes
"""

[<Test>]
let ``disable blank lines inside type constructor`` () =
    formatSourceString
        false
        """
type MNIST(path:string, ?urls:seq<string>, ?train:bool, ?transform:Tensor->Tensor, ?targetTransform:Tensor->Tensor) =
    inherit Dataset()
    let path = Path.Combine(path, "mnist") |> Path.GetFullPath
    let train = defaultArg train true
    let transform = defaultArg transform (fun t -> (t - 0.1307) / 0.3081)
    let targetTransform = defaultArg targetTransform id
    let urls = List.ofSeq <| defaultArg urls (Seq.ofList
                   ["http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz";
                    "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz";
                    "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz";
                    "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"])
    let files = [for url in urls do Path.Combine(path, Path.GetFileName(url))]
    let filesProcessed = [for file in files do Path.ChangeExtension(file, ".tensor")]
    let data, target =
        Directory.CreateDirectory(path) |> ignore
        let mutable data = dsharp.zero()
        let mutable target = dsharp.zero()
        if train then
            if not (File.Exists(files.[0])) then download urls.[0] files.[0]
            if not (File.Exists(files.[1])) then download urls.[1] files.[1]
            if File.Exists(filesProcessed.[0]) then data <-    dsharp.load(filesProcessed.[0]) else data <-    MNIST.LoadMNISTImages(files.[0]); dsharp.save(data, filesProcessed.[0])
            if File.Exists(filesProcessed.[1]) then target <- dsharp.load(filesProcessed.[1]) else target <- MNIST.LoadMNISTLabels(files.[1]); dsharp.save(target, filesProcessed.[1])
        else
            if not (File.Exists(files.[2])) then download urls.[2] files.[2]
            if not (File.Exists(files.[3])) then download urls.[3] files.[3]
            if File.Exists(filesProcessed.[2]) then data <-    dsharp.load(filesProcessed.[2]) else data <-    MNIST.LoadMNISTImages(files.[2]); dsharp.save(data, filesProcessed.[2])
            if File.Exists(filesProcessed.[3]) then target <- dsharp.load(filesProcessed.[3]) else target <- MNIST.LoadMNISTLabels(files.[3]); dsharp.save(target, filesProcessed.[3])
        data, target
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MNIST
    (
        path: string,
        ?urls: seq<string>,
        ?train: bool,
        ?transform: Tensor -> Tensor,
        ?targetTransform: Tensor -> Tensor
    ) =
    inherit Dataset()
    let path = Path.Combine(path, "mnist") |> Path.GetFullPath
    let train = defaultArg train true
    let transform = defaultArg transform (fun t -> (t - 0.1307) / 0.3081)
    let targetTransform = defaultArg targetTransform id
    let urls =
        List.ofSeq
        <| defaultArg
            urls
            (Seq.ofList
                [ "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"
                  "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"
                  "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"
                  "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz" ])
    let files =
        [ for url in urls do
              Path.Combine(path, Path.GetFileName(url)) ]
    let filesProcessed =
        [ for file in files do
              Path.ChangeExtension(file, ".tensor") ]
    let data, target =
        Directory.CreateDirectory(path) |> ignore
        let mutable data = dsharp.zero ()
        let mutable target = dsharp.zero ()
        if train then
            if not (File.Exists(files.[0])) then
                download urls.[0] files.[0]
            if not (File.Exists(files.[1])) then
                download urls.[1] files.[1]
            if File.Exists(filesProcessed.[0]) then
                data <- dsharp.load (filesProcessed.[0])
            else
                data <- MNIST.LoadMNISTImages(files.[0])
                dsharp.save (data, filesProcessed.[0])
            if File.Exists(filesProcessed.[1]) then
                target <- dsharp.load (filesProcessed.[1])
            else
                target <- MNIST.LoadMNISTLabels(files.[1])
                dsharp.save (target, filesProcessed.[1])
        else
            if not (File.Exists(files.[2])) then
                download urls.[2] files.[2]
            if not (File.Exists(files.[3])) then
                download urls.[3] files.[3]
            if File.Exists(filesProcessed.[2]) then
                data <- dsharp.load (filesProcessed.[2])
            else
                data <- MNIST.LoadMNISTImages(files.[2])
                dsharp.save (data, filesProcessed.[2])
            if File.Exists(filesProcessed.[3]) then
                target <- dsharp.load (filesProcessed.[3])
            else
                target <- MNIST.LoadMNISTLabels(files.[3])
                dsharp.save (target, filesProcessed.[3])
        data, target
"""

[<Test>]
let ``computation expressions`` () =
    formatSourceString
        false
        """
let comp =
    eventually { for x in 1 .. 2 do
                    printfn " x = %d" x
                 return 3 + 4 }"""
        config
    |> prepend newline
    |> should
        equal
        """
let comp =
    eventually {
        for x in 1..2 do
            printfn " x = %d" x
        return 3 + 4
    }
"""

[<Test>]
let ``recursive types`` () =
    formatSourceString
        false
        """
type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
"""

[<Test>]
let ``multiline recursive types`` () =
    formatSourceString
        false
        """
type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = ViewBinding<'model,'msg> list
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type ViewBinding<'model, 'msg> = string * Variable<'model, 'msg>
and ViewBindings<'model, 'msg> = ViewBinding<'model, 'msg> list
and Variable<'model, 'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model, 'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model, 'msg>
    | BindCmd of Execute<'model, 'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model, 'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""

[<Test>]
let ``recursive types in signature file`` () =
    formatSourceString
        true
        """
namespace Foobar

type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foobar

type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
"""
