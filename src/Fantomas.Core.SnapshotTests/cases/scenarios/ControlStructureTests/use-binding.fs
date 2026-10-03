    let writetofile filename obj =
     use file1 = File.CreateText(filename)
     file1.WriteLine("{0}", obj.ToString())
    