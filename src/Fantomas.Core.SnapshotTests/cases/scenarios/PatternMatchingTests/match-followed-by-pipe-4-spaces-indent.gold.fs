(match x with
 | Foo f ->
     "\n"
     + columnHeadersText
     + "\n"
     + seprator
     + "\n"
     + itemsText
 | Bar x ->
     // comment
     "")
|||> Some
