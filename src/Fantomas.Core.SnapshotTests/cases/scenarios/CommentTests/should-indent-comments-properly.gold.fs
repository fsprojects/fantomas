/// Non-local information related to internals of code generation within an assembly
type IlxGenIntraAssemblyInfo =
    {
        /// A table recording the generated name of the static backing fields for each mutable top level value where
        /// we may need to take the address of that value, e.g. static mutable module-bound values which are structs. These are
        /// only accessible intra-assembly. Across assemblies, taking the address of static mutable module-bound values is not permitted.
        /// The key to the table is the method ref for the property getter for the value, which is a stable name for the Val's
        /// that come from both the signature and the implementation.
        StaticFieldInfo: Dictionary<ILMethodRef, ILFieldSpec>
    }
