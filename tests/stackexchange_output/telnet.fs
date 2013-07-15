/// The program is from http://codereview.stackexchange.com/q/15364
module Telnet

open System
open System.Net.Sockets

let asyncGetInput = async { return BitConverter.GetBytes(Console.Read()) }

let rec asyncSendInput (stream : NetworkStream) = 
  async { 
    let! input = asyncGetInput
    stream.WriteByte
    |> Array.map <| input
    |> ignore
    do! asyncSendInput stream }

let asyncGetResponse (stream : NetworkStream) = 
  async { return Char.ConvertFromUtf32(stream.ReadByte()) }

let rec asyncPrintResponse (stream : NetworkStream) = 
  async { 
    let! response = asyncGetResponse stream
    Console.Write(response)
    do! asyncPrintResponse stream }

[<EntryPoint>]
let main args = 
  let client = new System.Net.Sockets.TcpClient()
  client.Connect(args.[0], Int32.Parse(args.[1]))
  printfn "Connected to %A %A..." args.[0] args.[1]
  let stream = client.GetStream()
  printfn "Got stream, starting two way asynchronous communication."
  Async.Parallel [asyncSendInput stream
                  asyncPrintResponse stream]
  |> Async.RunSynchronously
  |> ignore
  0
