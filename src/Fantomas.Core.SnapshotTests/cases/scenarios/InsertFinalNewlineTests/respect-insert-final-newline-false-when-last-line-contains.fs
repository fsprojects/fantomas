(*---
insert_final_newline = false
---*)
let mode =
    #if DEBUG
        "dev"
    #else
        "prod"
    #endif
