(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
---*)
let person =
    { Name = "James"
      Address = { Street = "Bakerstreet"; Number = 42 }  // end address
    } // end person
