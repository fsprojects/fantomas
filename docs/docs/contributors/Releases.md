---
category: Contributors
categoryindex: 2
index: 14
---
# Releases

Releases in Fantomas are not automated and require some manual steps by the maintainers.

## Preparation

The [CHANGELOG.md](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md) needs a new header with an official release tag:

```md
## [5.1.0] - 2022-11-04
```

It is custom to have the next version merged into the `main` branch and locally publish from there.

Verify that all recent PRs and closed issues are listed in the changelog. Normally, this should be ok as we require a changelog entry before we merge a PR.

## NuGet push

To publish the new versions to NuGet:

> dotnet fsi build.fsx --push

The `--push` will try and publish the `*.nupkg` files created by the build in the `bin` folder.
`Fantomas.Client` will be excluded and requires a specific pipeline to publish.

The pipeline does assume that the `NUGET_KEY` environment variable is set with a valid NuGet key.

## GitHub release

A new GitHub release entry should be created for official versions. This will notify users who have subscribed to releases.  
In the past some alpha or beta releases have had a GitHub release, it depends on the occasion.

### Tag

Create a new tag based on the main branch. The assumption is that the `CHANGELOG` file contains the tag version you are about to create.

### Release title

For a revision release you can use the current date (example: `October 13th Release`), for a minor or major releas pick the month name (example: `September`).

### Description

Some parts of the description are fixed, some depend on the occasions:

```md
# Title

<!-- Optional intro paragraph  -->

<!-- Copy the sections of the changelog -->

<!-- Optional special thanks -->
Special thanks to @x, @y and @z!

<!-- Link to published version on NuGet -->
[https://www.nuget.org/packages/fantomas/5.0.4](https://www.nuget.org/packages/fantomas/5.0.4)
```

The format of the title is `# version` (example: `# 5.0.5`). This differs from the notation used in the changelog file!  
For minors and majors a codename (inside a `<sub>` tag) is used. All codenames so far have been song titles by the band Ghost (example `# 5.1.0 <sub>Kaisarion</sub> - 11/2022`).  
With the exception of `v5, "Get Back"`.

The list of people to thank is compiled by cross referencing the changelog entries. The author of the GitHub release is omitted.  
Don't be shy to include other names of people who have contributed in alternative ways when the occasion calls for it.

### Artifacts

Upload the `*.nupkg` files to the release artifacts.

## Spread the word

Share the newly created release on our Discord server in the `#announcements` channel.  
Optionally share (minor or major) releases on other social media. 

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>