# Measurements by hand

Two kinds of claim take a command `compare.fsx` does not run: what a released version does, and what
the SDK compiler accepts. Both are recorded the same way, so the rerun in step 6 compares them.

## Recording one

Write the command to `tmp/pr-description/<n>/commands/<name>.sh`, then run it with
`sh tmp/pr-description/<n>/commands/<name>.sh`, as the rerun does. The script:

- moves by its absolute path into `tmp/pr-description/<n>`, or into its `compile/` folder for a
  compile check (`dotnet` takes `global.json` from the folder it runs in);
- starts from a fresh copy of its input each time;
- writes everything a reader needs to `out/<name>`: what the tool or compiler printed, its exit code,
  and for a released version the formatted file;
- keeps local paths out of that output, through `sed`, the way `compare.fsx` prints its sample
  folder as `sample`.

Inputs sit in `inputs/<name>/` beside their `.editorconfig`, as step 3 of `SKILL.md` lays out, and
an input for a released version also gets an empty `.fantomasignore`, so the repository's ignore file
stays out of the run. A compile check's `.fsx` is the exception: it sits in `compile/` itself, and is
compiled where it is.

## Released versions

Run the latest release of each major the claim names, or attribute the claim to the issue it came
from. A released version formats in place, and reads a case's front matter as a trivia block comment,
so the copy goes without the front matter and its settings go in the `.editorconfig`.

```sh
cd <repository>/tmp/pr-description/<n>
rm -rf run/x-7 && mkdir -p run/x-7 out && cp -r inputs/x/. run/x-7/
cd run/x-7
dotnet tool exec -y fantomas@7.0.6 -- --version > /dev/null 2>&1
{ dotnet tool exec -y fantomas@7.0.6 -- x.fs 2>&1; echo "-- exit $?"; cat x.fs; } | sed "s|$PWD|sample|g" > ../../out/x-7
```

The `--version` run fetches the tool first, so whatever a download prints stays out of `out/x-7` and
the rerun sees the same output. The run happens inside the copy, so a path the tool prints starts
with the folder `sed` turns into `sample`. A version that refuses its own result leaves the file unchanged, and
its message and exit code tell that apart from a result that did not change.

## What compiles

"Valid" in `compare.fsx`'s status is Fantomas' own check, which accepts code the compiler can reject.
A claim that code compiles, or does not, takes the SDK compiler, the way `AGENTS.md` asks under
"Every example in full": a `.fsx` in `tmp/pr-description/<n>/compile/`, beside a `global.json`
pinning the newest stable SDK installed (a preview compiler accepts more), with stubs for the types
it uses.

```sh
cd <repository>/tmp/pr-description/<n>/compile
mkdir -p ../out
{ dotnet fsi --quiet option-c.fsx 2>&1; echo "-- exit $?"; } | sed "s|$PWD/||g" > ../out/option-c
```

The description quotes an error from its `error FS` on: the script's name is scratch too.
