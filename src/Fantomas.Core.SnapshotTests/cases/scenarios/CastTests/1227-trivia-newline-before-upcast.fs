module S3v2

open System.Threading.Tasks
open Amazon.Runtime

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously
    x.Result :> AmazonWebServiceResponse

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :> AmazonWebServiceResponse
