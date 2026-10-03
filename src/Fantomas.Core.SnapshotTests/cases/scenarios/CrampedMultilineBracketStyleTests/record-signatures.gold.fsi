module RecordSignature

/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes: IDictionary<Name, string>

        /// The children collection.
        Children: seq<INode>

        /// The qualified name.
        Name: Name
    }

    interface INode

    /// Constructs an new empty Element.
    static member Create: name: string * ?uri: string -> Element

    /// Replaces the children.
    static member WithChildren: children: #seq<#INode> -> self: Element -> Element

    /// Replaces the children.
    static member (-): self: Element * children: #seq<#INode> -> Element

    /// Replaces the attributes.
    static member WithAttributes: attrs: #seq<string * string> -> self: Element -> Element

    /// Replaces the attributes.
    static member (+): self: Element * attrs: #seq<string * string> -> Element

    /// Replaces the children with a single text node.
    static member WithText: text: string -> self: Element -> Element

    /// Replaces the children with a single text node.
    static member (--): self: Element * text: string -> Element
