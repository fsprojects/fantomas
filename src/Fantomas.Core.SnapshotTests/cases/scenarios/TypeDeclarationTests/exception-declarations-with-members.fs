(*---
fsharp_max_infix_operator_expression = 60
fsharp_max_function_binding_width = 120
---*)
/// An exception type to signal build errors.
exception BuildException of string*list<string>
  with
    override x.ToString() = x.Data0.ToString() + "\r\n" + (separated "\r\n" x.Data1)