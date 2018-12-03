/// The program is from http://codereview.stackexchange.com/q/21374
module Color

open System.IO
open System.Net


/// currently the color table is create via the AddColor method, however
/// the initial values should be created with the loadrgb and colorinfo members
type MyFSColorTable() = 

  /// pull the X11 rgb.txt color table off the web in text format
  static let loadrgb =
    let url = "http://people.csail.mit.edu/jaffer/Color/rgb.txt"
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let txt = reader.ReadToEnd()
    txt

  /// parse the text of the rgb.txt color table into a Name: Values: list
  static let colorinfo =
      loadrgb.Split([|'\n'|])
      |> Seq.skip 1
      |> Seq.map (fun line -> line.Split([|'\t'|]))        
      |> Seq.filter (fun values -> values |> Seq.length = 3)
      |> Seq.map (fun values -> string values.[0], string values.[2])
      |> Seq.map (fun (rgb, name) -> rgb.Split([|' '|]), name)
      |> Seq.map (fun (rgb, name) -> [|name, rgb.[0], rgb.[1], rgb.[2]|])

  /// Mutable Color Table will be defined on-the-fly
  let mutable ColorTable = []
  /// Euclidean distance between 2 vectors - float is overkill here
  static let Dist (V1: float[]) V2 =
    Array.zip V1 V2
      |> Array.map (fun (v1, v2) -> pown (v1 - v2) 2)
      |> Array.sum