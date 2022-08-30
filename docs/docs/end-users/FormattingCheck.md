---
category: End-users
categoryindex: 1
index: 5
---

# Formatting Check

Formatting source code is a habit, a step in your developer workflow.  
The benefits of consistently formatting is that **your delta** (*typically the changes in the files of a pull request*) will be the **smallest set possible** if every previous change was formatted.

## The tragedy of the ancient Greek developers

When working with multiple people on the same code base, it is important (to some degrees) that you cannot distinguish who wrote the code.
If you agree as a team that you need to write unit tests to guarantee the code quality, you expect every developer to cover any new code with a test.  
Formatting is no different, every developer should do it and it is an acceptance criteria for new code.

Imagine we have multiple developers in our team. Hektor made the initial setup and during a team meeting it was decided that the code should always be formatted.  
Once the initial project structure was delivered, Achilles made a change where the code was not formatted.  
The next day, Odysseus wants to submit a new pull request. As agreed code should be formatted, so Odysseus did exactly that.
As the changes Achilles made were not formatted, there was more code touched than was absolutely necessary. Perseus didn't see any harm in this rectification and merge the pull request as is.

A week later, a huge problem was discovered in production. There was data loss and the entire company was on fire.
Fuelled by rage and anger Hektor wanted to know who was responsible for this devastating tragedy. You guessed it, somebody's head was about to roll.
Perseus was tasked with getting to the bottom of this mystery and eventually he located the source of the misery.  Hektor tasked him with running a `git blame` command, to see who the culprit really was.

Odysseus credentials showed up, but it was the function Achilles originally wrote. Unable to escape Hektor's wrath, Odysseus was fired immediately.
Achilles felt bad but couldn't come clean as he was about to get surgery for his heel. Odysseus took the fall like a true hero.
But it lead him to a downwards spiral and would take him ten years before he could land another job in the software industry.

### Aftermath

The team was shocked by what had transpired. Besides Hektor overreacting, another painful meeting was planned to have a retrospective on the past events.  
After asking five why's, the team had to brainstorm on how to avoid these things.

Ultimately, that meeting was mostly about the contents of the function Achilles wrote, but to end on a high note, the team discussed formatting source code afterwards.

The moral of this story is that there are two things that could have saved Odysseus: **a formatting check during continuous integration** and a `.git-blame-ignore-revs` file.

## --check

*starting version 3.3*

Verify that a single file or folder was formatted correctly.

> dotnet fantomas --check Source.fs

This will verify if the file `Source.fs` still needs formatting.
If it does, the process will return exit code **99**.
In the case that the file does not require any formatting, exit code 0 is returned.
Unexpected errors will return exit code 1.

This scenario is meant to be executed in a continuous integration environment, to enforce that the newly added code was formatted correctly.

### FAKE

If you are using `FAKE` by any chance, a good step is to have `CheckFormat` target early on in the pipeline.

```fsharp
Target.create "CheckFormat" (fun _ ->
    let result =
        DotNet.exec id "fantomas ourSourceFolder --recurse --check"

    if result.ExitCode = 0 then
        Trace.log "No files need formatting"
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, run \"dotnet fantomas  ourSourceFolder --recurse\" to resolve this."
    else
        Trace.logf "Errors while formatting: %A" result.Errors)
        
// ... more targets

"Clean"
==> "CheckFormat"
==> "Build"
==> "UnitTests"
==> "Benchmark"
==> "Pack"
==> "Docs"
==> "All"

Target.runOrDefault "All"
```

The recommendation is to install `fantomas` as a local tool and run it using the generic `DotNet.exec` api.
This translates to running `dotnet fantomas ourSourceFolder --recurse --check`.

### Any other continuous integration environment

You want to restore your local `fantomas` tool using `dotnet tool restore`.
Next, you want to run `dotnet fantomas <input> --check` and make sure your continuous integration environment **fails** your job when **a non-zero exit code** is returned.

**Pro-tip**: print the command users need to run to fix the formatting in the output log when the check failed. This is useful for open-source projects where new contributors might never have been exposed to formatting.

## A git-blame-ignore-revs file

By default, Fantomas adheres to the Microsoft [F# code formatting guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting).
If these change, Fantomas will follow accordingly. Due to this reason, the output cannot be guaranteed to remain the same when upgrading to a new minor version.

If you are using Git for your source control, it is recommended to ignore commits where `fantomas` was updated using a [.git-blame-ignore-revs file](https://git-scm.com/docs/git-blame#Documentation/git-blame.txt---ignore-revltrevgt).
Check out this [blogpost](https://www.moxio.com/blog/43/ignoring-bulk-change-commits-with-git-blame) for more details.

Adding a formatting commit to a configured `.git-blame-ignore-revs` file will prevent you from drawing the wrong conclusions when running a `git blame` command.
One thing to note is that if you add a commit SHA to a `.git-blame-ignore-revs` file, you cannot squash the commit when merging in the pull request.

## Checking is good for regressions

Normally, the rule of thumb is that the code style will not change between revisions.  
If you are using `4.7.2`, then it should be safe for you upgrade to the latest `4.7.X` without seeing any changes.

> Life happens, so this is a best effort guarantee

In case you do see a change that cannot be linked to anything in the [CHANGELOG.md](https://github.com/fsprojects/fantomas/blob/master/CHANGELOG.md) file, you may have detected a regression.  
Or, more likely, you have something slightly different in your code base that isn't covered yet by a unit test.

No matter the case, when you have a `--check` command somewhere in your continuous integration environment, please consider running a build with the latest compatible version from time to time.  
It really helps us spotting problems early on and we can more easily pinpoint the problem due to lesser recent changes.

<div class="d-flex justify-content-between my-4">
    <a href="./IgnoreFiles.html">Previous</a>
</div>