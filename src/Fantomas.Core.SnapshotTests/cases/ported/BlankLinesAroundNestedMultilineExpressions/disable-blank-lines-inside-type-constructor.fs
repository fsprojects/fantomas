(*---
max_line_length = 100
fsharp_blank_lines_around_nested_multiline_expressions = false
fsharp_multiline_bracket_style = cramped
---*)
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
