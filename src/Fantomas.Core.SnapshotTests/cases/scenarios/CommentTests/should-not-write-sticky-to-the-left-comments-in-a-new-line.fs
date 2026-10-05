let moveFrom source =
  getAllFiles source
    |> Seq.filter (fun f -> Path.GetExtension(f).ToLower() <> ".db")  //exlcude the thumbs.db files
    |> move @"C:\_EXTERNAL_DRIVE\_Camera"
