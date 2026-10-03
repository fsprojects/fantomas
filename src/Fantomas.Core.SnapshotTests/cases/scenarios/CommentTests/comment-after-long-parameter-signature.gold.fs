type CreateFSharpManifestResourceName public () =
    inherit CreateCSharpManifestResourceName()

    member val UseStandardResourceNames = false with get, set

    override this.CreateManifestName
        (
            fileName: string,
            linkFileName: string,
            rootNamespace: string, // may be null
            dependentUponFileName: string, // may be null
            binaryStream: Stream // may be null
        ) : string =
        ()
