/// The program is from http://codereview.stackexchange.com/q/20068
module Performance

open System

open System.Drawing

open System.IO

open System.Text

open System.Text.RegularExpressions

let rec getAllFiles baseDir = 
  seq { 
    yield! Directory.EnumerateFiles(baseDir)
    for dir in Directory.EnumerateDirectories(baseDir) do
      yield! getAllFiles dir }

let dateTakenFromExif file = 
  let r = new Regex(":")
  use fs = new FileStream(file, FileMode.Open, FileAccess.Read)
  use myImage = Image.FromStream(fs, false, false)
  let propItem = myImage.GetPropertyItem(36867)
  let dateTaken = r.Replace(Encoding.UTF8.GetString(propItem.Value), "-", 2)
  DateTime.Parse(dateTaken)

let getDateTaken file = 
  try 
    dateTakenFromExif file
  with
  | :? Exception -> File.GetLastWriteTime(file)

let addToFile file n = 
  match n with
  | 2 -> 
    let fileDir = Path.GetDirectoryName(file)
    let nextFile = Path.GetFileNameWithoutExtension(file) + "-" + n.ToString() + Path.GetExtension(file)
    Path.Combine(fileDir, nextFile)
  | _ -> 
    let prev = n - 1
    file.Replace("-" + prev.ToString(), "-" + n.ToString())

let getNewFilename newFilePath = 
  let rec incrementFilename file n = 
    let filenameIncremented = addToFile file n
    match File.Exists(filenameIncremented) with
    | false -> filenameIncremented
    | true -> incrementFilename filenameIncremented (n + 1)
  match File.Exists(newFilePath) with
  | false -> newFilePath
  | true -> incrementFilename newFilePath 2

let move destinationRoot files = 
  let moveHelper file = 
    let dateTaken = getDateTaken file
    let finalPath = Path.Combine(destinationRoot, dateTaken.Year.ToString(), dateTaken.ToString("yyyy-MM-dd"))
    if not(Directory.Exists(finalPath))
    then Directory.CreateDirectory(finalPath)
         |> ignore
    let newFile = getNewFilename(Path.Combine(finalPath, Path.GetFileName(file)))
    try 
      File.Copy(file, newFile)
    with
    | :? Exception as e -> failwith(sprintf "error renaming %s to %s\n%s" file newFile e.Message)
  files
  |> Seq.iter moveHelper

let moveFrom source = 
  getAllFiles source
  |> Seq.filter(fun f -> Path.GetExtension(f).ToLower() <> ".db")
  |> move """C:\_EXTERNAL_DRIVE\_Camera"""
  printfn "Done"

moveFrom """C:\Users\Mike\Pictures\To Network"""
moveFrom """C:\_EXTERNAL_DRIVE\Camera"""