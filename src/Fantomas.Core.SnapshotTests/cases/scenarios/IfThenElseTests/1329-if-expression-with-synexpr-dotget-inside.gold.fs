let private tryGetUrlWithExactMatch
  (pathPattern: string<SourcelinkPattern>)
  (urlPattern: string<Url>)
  (document: Document)
  =
  if (UMX.untag pathPattern).Equals(UMX.untag document.Name, System.StringComparison.Ordinal) then
    Some(urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document)
  else
    None

let private tryGetUrlWithExactMatch
  (pathPattern: string<SourcelinkPattern>)
  (urlPattern: string<Url>)
  (document: Document)
  =
  if (UMX.untag pathPattern).Equals(UMX.untag document.Name, System.StringComparison.Ordinal) then
    Some(urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document)
  else
    None
