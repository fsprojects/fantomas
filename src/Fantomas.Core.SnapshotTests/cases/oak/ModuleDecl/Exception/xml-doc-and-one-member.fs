(*---
# `with` moves up to the end of the case line, and the member is indented below it.
---*)
/// An exception type to signal build errors.
exception BuildException of string*list<string>
  with
    override x.ToString() = x.Data0
