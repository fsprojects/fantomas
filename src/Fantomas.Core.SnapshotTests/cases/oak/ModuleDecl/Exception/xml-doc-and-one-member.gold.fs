/// An exception type to signal build errors.
exception BuildException of string * list<string> with
    override x.ToString() = x.Data0
