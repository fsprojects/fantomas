/// IL Method definitions.
[<NoComparison; NoEquality>]
type ILMethodDef =

    /// Functional creation of a value, with delayed reading of some elements via a metadata index
    internal new:
        name: string *
        attributes: MethodAttributes *
        implAttributes: MethodImplAttributes *
        callingConv: ILCallingConv *
        parameters: ILParameters *
        ret: ILReturn *
        body: Lazy<MethodBody> *
        isEntryPoint: bool *
        genericParams: ILGenericParameterDefs *
        securityDeclsStored: ILSecurityDeclsStored *
        customAttrsStored: ILAttributesStored *
        metadataIndex: int32 ->
            ILMethodDef
