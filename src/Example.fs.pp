module $rootnamespace$.Example

open System
open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = { FormatConfig.Default with 
                IndentSpaceNum = 2 }

let source = "
    let Multiple9x9 () = 
      for i in 1 .. 9 do
        printf \"\\n\";
        for j in 1 .. 9 do
          let k = i * j in
          printf \"%d x %d = %2d \" i j k;
          done;
      done;;
    Multiple9x9 ();;"

Console.WriteLine("Input:\n{0}\n", source)
Console.WriteLine("Result:\n{0}\n", formatSourceString source config)

Console.WriteLine("Press any key to finish...")
Console.ReadKey() |> ignore
