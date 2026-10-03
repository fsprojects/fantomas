/// Not all custom attribute data can be decoded without binding types.  In particular
/// enums must be bound in order to discover the size of the underlying integer.
/// The following assumes enums have size int32.
val internal decodeILAttribData:
    ILAttribute ->
        ILAttribElem list (* fixed args *) *
        ILAttributeNamedArg list (* named args: values and flags indicating if they are fields or properties *)

val internal mkILCustomAttribMethRef:
    ILMethodSpec *
    ILAttribElem list (* fixed args: values and implicit types *) *
    ILAttributeNamedArg list (* named args: values and flags indicating if they are fields or properties *) ->
        ILAttribute

val pdbReaderGetMethodFromDocumentPosition: PdbReader -> PdbDocument -> int (* line *) -> int (* col *) -> PdbMethod
