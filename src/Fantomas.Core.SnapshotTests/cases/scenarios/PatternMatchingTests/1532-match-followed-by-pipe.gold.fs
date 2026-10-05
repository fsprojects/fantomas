(match x with
 | Foo f -> []
 | Bar x ->
   "\n"
   + columnHeadersText
   + "\n"
   + seprator
   + "\n"
   + itemsText)
|> Some
