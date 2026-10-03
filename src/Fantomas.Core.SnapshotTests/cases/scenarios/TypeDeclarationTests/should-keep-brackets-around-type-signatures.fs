(*---
fsharp_max_value_binding_width = 50
---*)
let user_printers = ref([] : (string * (term -> unit)) list)
let the_interface = ref([] : (string * (string * hol_type)) list)
    