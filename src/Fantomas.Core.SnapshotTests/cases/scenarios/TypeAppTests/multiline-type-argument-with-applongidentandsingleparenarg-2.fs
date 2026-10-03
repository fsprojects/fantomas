(*---
max_line_length = 30
fsharp_space_before_class_constructor = true
---*)
path.Replace<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >("../../../", "....")
