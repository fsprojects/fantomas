---
category: Contributors
categoryindex: 2
index: 3
---
# Getting started

Fantomas has a fairly straightforward setup.

## Recommended workflow

We recommend the following overall workflow when developing for this repository:

* Fork this repository
* Always work in your fork
* Always keep your fork up to date

Before updating your fork, run this command:

```shell
git remote add upstream https://github.com/fsprojects/fantomas.git
```

This will make management of multiple forks and your own work easier over time.

## Updating your fork

We recommend the following commands to update your fork:

```shell
git checkout master
git clean -xdf
git fetch upstream
git rebase upstream/master
git push
```

Or more succinctly:

```shell
git checkout master && git clean -xdf && git fetch upstream && git rebase upstream/master && git push
```

This will update your fork with the latest from `fsprojects/fantomas` on your machine and push those updates to your remote fork.

## Initial build

After cloning the repository, you can restore the local .NET tools:

```shell
dotnet tool restore
```

Next, you should run the default FAKE target.
This will build the solution, run all unit tests and do everything that the CI build does.

```shell
dotnet fake build
```

If you are unfamiliar with [FAKE](https://fake.build/), you can read the [FAKE getting started guide](https://fake.build/fake-gettingstarted.html).  
To see what other targets are available, run:

```shell
dotnet fake run build.fsx --list
```

Next, you should run a FAKE target that sets up some git repo-level configuration.

```shell
dotnet fake build -t EnsureRepoConfig
```

This target makes changes to the local git repository configuration to ensure
that formatting of new code is consistent before it is pushed up to a remote repository.

<fantomas-nav previous="./FSharp.html" next="./Solution%20Structure.html">