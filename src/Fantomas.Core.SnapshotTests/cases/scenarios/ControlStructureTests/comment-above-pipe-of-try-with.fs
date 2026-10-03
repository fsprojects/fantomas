try
    let defaultTime = (DateTime.FromFileTimeUtc 0L).ToLocalTime ()
    foo.CreationTime <> defaultTime
with
// hmm
| :? FileNotFoundException -> false
