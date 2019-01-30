module Fantomas.Tests.FakeHelpersTests

open NUnit.Framework
open System
open System.IO
open Fantomas.FakeHelpers
open Fantomas.FormatConfig

[<Test>]
let ``checkCode test`` () =
    let sourceCode =
        """
module SampleCode

open System.IO
open System

type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString () =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s

let Multiple9x9 () =
    for i in 1 .. 9 do
        printf "\n";
        for j in 1 .. 9 do
            let k = i * j in
            printf "%d x %d = %2d " i j k;
        done;
    done;;

let getTempPath () =
    Guid.NewGuid().ToString("N") |> sprintf "%s.png" 
    |> fun g -> Path.Combine("SomeFolder", g)
"""
    let tempPath =
        let fileName = sprintf "%s.fs" (Guid.NewGuid().ToString("N"))
        let tempFolder = Path.GetTempPath()
        Path.Combine(tempFolder, fileName)
      
    File.WriteAllText(tempPath, sourceCode)
    
    try
        checkCode FormatConfig.Default (Seq.singleton tempPath)
    finally
        File.Delete(tempPath)