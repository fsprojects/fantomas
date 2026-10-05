type StreamHelper() =
    inherit Stream()

    override x.ReadAsync(dst, offset, count, tok) = ()
    override x.WriteAsync(dst, offset, count, tok) = ()
    override x.Flush() = ()
    override x.Seek(offset: int64, origin: SeekOrigin) = ()
    override x.SetLength(value: int64) = ()
    override x.Read(dst, offset, count) = ()
    override x.Write(src, offset, count) = ()
    override x.ReadByte() = ()
    override x.WriteByte item = ()
    override x.CanRead = true
    override x.CanSeek = false
    override x.CanWrite = true
    override x.Length = 1

    override x.Position
        with get () = 1
        and set value = 1

    override x.Dispose disposing = ()
