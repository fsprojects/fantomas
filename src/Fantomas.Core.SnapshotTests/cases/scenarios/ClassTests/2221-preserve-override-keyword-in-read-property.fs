type StreamHelper() =
    inherit Stream()

    override x.ReadAsync (dst, offset, count, tok) = 
        ()
    override x.WriteAsync (dst, offset, count, tok) = 
        ()
    override x.Flush () = ()
    override x.Seek(offset:int64, origin:SeekOrigin) =
        ()
    override x.SetLength(value:int64) =
        ()
    override x.Read(dst, offset, count) = 
        ()           
    override x.Write(src, offset, count) = 
        ()
    override x.ReadByte() =
        ()
    override x.WriteByte item =
        ()
    override x.CanRead 
        with get() =
            true
    override x.CanSeek
        with get() =
            false
    override x.CanWrite
        with get() =
            true
    override x.Length
        with get() =
            1
    override x.Position
        with get() =
            1
        and set value =
            1
    override x.Dispose disposing =
        ()
