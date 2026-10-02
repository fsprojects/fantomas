open System.Collections.Generic

let inventory = Dictionary<string, int>()

inventory.Add("Apples", 1)
inventory.Add("Oranges", 2)
inventory.Add("Bananas", 3)

inventory["Oranges"] // raises an exception if not found
inventory.["Apples"] // raises an exception if not found
nestedInventory["Oranges"][23] // raises an exception if not found
