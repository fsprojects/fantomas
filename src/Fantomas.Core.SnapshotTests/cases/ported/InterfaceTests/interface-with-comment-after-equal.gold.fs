/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage: string
