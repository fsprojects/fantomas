/// The program is from http://codereview.stackexchange.com/q/2665
module Comments

open System.IO

type Comment = 
  { Author : string
    Body : string }

let parseComment (line : string) = 
  match line.Split(';') with
  | [|author; body|] -> 
    Some({ Author = author
           Body = body })
  | _ -> None

let filterOutNone maybe = 
  match maybe with
  | Some(_) -> true
  | _ -> false

let makeSome some = 
  match some with
  | Some(v) -> v
  | _ -> failwith "error"

let readAllComments() = 
  File.ReadAllLines("comments.txt")
  |> Array.map parseComment
  |> Array.filter filterOutNone
  |> Array.map makeSome
