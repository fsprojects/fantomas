(*---
fsharp_multiline_bracket_style = cramped
---*)
namespace web_core

open WebSharper.UI

module Maintoc =
  let Page =
    { MyPage.Create() with body =
                                [ Doc.Verbatim """
This is a very long line in a multi-line string, so long in fact that it is longer than that page width to which I am trying to constrain everything, and so it goes bang.
""" ] }