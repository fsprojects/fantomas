# Landing page hero recording

`hero.tape` is a [VHS](https://github.com/charmbracelet/vhs) script that records the terminal
session behind the hero of `docs/index.html`, so the footage can be re-rendered whenever the CLI
output changes instead of going stale.

```bash
brew install vhs
dotnet build src/Fantomas/Fantomas.fsproj
vhs scripts/hero-video/hero.tape
```

Run it from the repository root. `fixture/` holds the deliberately unformatted project the
recording formats; it is listed in `.fantomasignore` so the repo's own formatting check leaves it
alone. `bin/fantomas` puts the local debug build on `PATH` under its plain name.
