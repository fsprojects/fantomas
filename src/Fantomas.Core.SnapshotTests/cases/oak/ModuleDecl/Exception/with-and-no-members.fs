(*---
# `with end` without members is dropped.
---*)
exception Foo of int with
    end
