(*---
# The comment after the last parameter stays before the closing parenthesis, because the comment after the first already makes the list multiline.
---*)
[<DllImport("x")>]
extern void f(
    int a, // first
    int b // second
)
