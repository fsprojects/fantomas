(*---
fsharp_max_infix_operator_expression = 50
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
  let PrepareReadMe packingCopyright =
    let readme = Path.getFullName "README.md"
    let document = File.ReadAllText readme
    let markdown = Markdown()
    let docHtml = """<?xml version="1.0"  encoding="utf-8"?>
<!DOCTYPE html>
<html lang="en">
<head>
<title>AltCover README</title>
<style>
body, html {
color: #000; background-color: #eee;
font-family: 'Segoe UI', 'Open Sans', Calibri, verdana, helvetica, arial, sans-serif;
position: absolute; top: 0px; width: 50em;margin: 1em; padding:0;
}
a {color: #444; text-decoration: none; font-weight: bold;}
a:hover {color: #ecc;}
</style>
</head>
<body>
"""               + markdown.Transform document + """
<footer><p style="text-align: center">""" + packingCopyright + """</p>
</footer>
</body>
</html>
"""
    let xmlform = XDocument.Parse docHtml
    let body = xmlform.Descendants(XName.Get "body")
    let eliminate = [ "Continuous Integration"; "Building"; "Thanks to" ]
    let keep = ref true

    let kill =
      body.Elements()
      |> Seq.map (fun x ->
           match x.Name.LocalName with
           | "h2" ->
               keep
               := (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate)
                  |> Option.isNone
           | "footer" -> keep := true
           | _ -> ()
           if !keep then None else Some x)
      |> Seq.toList
    kill
    |> Seq.iter (fun q ->
         match q with
         | Some x -> x.Remove()
         | _ -> ())
    let packable = Path.getFullName "./_Binaries/README.html"
    xmlform.Save packable
