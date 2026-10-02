if
    Uri.Compare(
        foo,
        bar,
        UriComponents.Host ||| UriComponents.Path,
        UriFormat.UriEscaped,
        StringComparison.CurrentCulture
    )
        =
        0
then
    ()
else
    ()
