module RecordSignature

/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes : IDictionary<Name, string>

        /// The children collection.
        Children : seq<INode>

        /// The qualified name.
        Name : Name
    }
