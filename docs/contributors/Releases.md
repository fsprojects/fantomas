# Releases

Releases in Fantomas are automated via GitHub Actions. When a new release entry is added to the [CHANGELOG.md](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md) and pushed to the `main` branch, the release workflow will automatically:

0 Build and test the project

1 Create NuGet packages

2 Publish packages to NuGet

3 Create a GitHub release with release notes

## Preparation

The [CHANGELOG.md](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md) needs a new header with an official release tag:

```md
## [5.1.0] - 2022-11-04
```

For prerelease versions, include the prerelease suffix:

```md
## [8.0.0-alpha-001] - 2024-12-12
```

Verify that all recent PRs and closed issues are listed in the changelog. Normally, this should be ok as we require a changelog entry before we merge a PR.

Once the changelog entry is merged into `main`, the release workflow will automatically trigger and handle the release process.

## Testing Releases Locally

You can test the release process locally using the `--dry-run` flag. This will perform all validation and generate release notes without actually publishing to NuGet or creating a GitHub release:

```bash
dotnet fsi build.fsx -- -p Release --dry-run
```

This is useful for:
- Verifying the release notes before publishing
- Checking that the changelog is parsed correctly
- Testing author attribution
- Ensuring the release pipeline works as expected

## Automated Release Process

The release pipeline (`build.fsx -p Release`) performs the following steps:

0 **Parses the changelog** to find the current and last release

1 **Checks if the release already exists** on GitHub (skips if already published)

2 **Builds and tests** the project

3 **Creates NuGet packages** for all projects (except `Fantomas.Client`)

4 **Publishes packages to NuGet**

5 **Generates release notes** including:

  * Changelog sections (Added, Changed, Fixed, etc.)
  
  * Contributor attribution (from PR commits merged since the last release)
  
  * Link to NuGet package
  

6 **Creates GitHub release**:

  * Draft releases for stable minor/major versions (patch = 0)
  
  * Published releases for revisions (patch &gt; 0) and all prereleases
  
  * Includes `--prerelease` flag for alpha/beta versions
  

### Release Types

* **Stable minor/major** (e.g., `7.0.0`, `8.0.0`): Created as draft releases, requiring manual publish

* **Stable revisions** (e.g., `7.0.5`, `8.1.2`): Published immediately

* **Prereleases** (e.g., `8.0.0-alpha-001`, `8.0.0-beta-001`): Always published immediately

### Author Attribution

The release notes automatically include contributor attribution by:
- Querying PRs merged since the last release
- Extracting authors from all commits in those PRs
- Filtering out bots (e.g., `dependabot[bot]`)
- Generating a "Special thanks to..." message

## Manual Steps

The only manual step required is for **minor and major releases**:

### Adding Release Nicknames

Minor and major releases are created as **draft releases** on GitHub. This allows maintainers to add a cool nickname to the release title before publishing.

The nickname is typically a song name from the band Ghost. For example:
- `# 5.1.0 <sub>Kaisarion</sub> - 11/2022`
- `# 7.0.0 <sub>Year Zero</sub> - 01/2025`

To add a nickname:
1. Go to the draft release on GitHub
2. Edit the release title to add the nickname in a `<sub>` tag
3. Review the release notes (they're already generated automatically)
4. Publish the release

**Note**: `Fantomas.Client` requires a separate pipeline (`build.fsx -p PushClient`) and is not included in the automated release.

## Spread the word

Share the newly created release on our Discord server in the `#announcements` channel.
Optionally share (minor or major) releases on other social media.

<fantomas-nav previous="Updating%20the%20compiler.md" next="Formatting%20Conventions.md"></fantomas-nav>