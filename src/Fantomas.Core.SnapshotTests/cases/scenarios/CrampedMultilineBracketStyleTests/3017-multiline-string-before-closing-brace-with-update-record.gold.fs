let FableSampleExpected: Changelogs =
    { Unreleased =
        Some
            { ChangelogData.Default with
                Fixed =
                    normalizeNewline
                        """#### Python
"""         }
      Releases =
        [ Some
              { ChangelogData.Default with
                  Changed = normalizeNewline "" } ] }
