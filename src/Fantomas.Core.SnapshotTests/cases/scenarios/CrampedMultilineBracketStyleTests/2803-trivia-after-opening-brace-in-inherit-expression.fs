(*---
fsharp_multiline_bracket_style = cramped
---*)
let range =
    { // foo
      // bar
      inherit // reason
        Foo()
      X = y
      Z =
        someReallyLongExpressionThatIsLongerThanTheLineLength
            aLongArgument
            //
            anotherLongArgument
            fooBar }
