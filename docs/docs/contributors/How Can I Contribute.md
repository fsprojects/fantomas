---
category: Contributors
categoryindex: 2
index: 9
---
# How can I contribute?

There are many ways to contribute to an open-source project. From linking a tweet to show some interest to solved heavy handed coding problems.   
The most obvious thing where we can use some help is fixing bugs, but there are a lot of other things that most certainly would be welcome.

## Bug fixes

The most welcome additions to the project are bug fixes. The philosophy behind this is that everyone should be able to fix their own bug.
If we can achieve this as a community, we can share the workload and all benefit together.
The project can move at a faster pace and improve as a whole.

We strongly encourage people to embrace the reward of solving their own problems.
We'll ask for a regression test when you fix a bug, to guarantee that you won't encounter the bug again.

### bug (soundness)

Our goal is for Fantomas to be able to format all files out of the box without breaking correctness.
It's very important to us that a new user's experience is smooth and at the very least results in correct code.
Bugs labelled `bug (soundness)` all indicate places where a new user might bounce off Fantomas because it actually broke their code.
We want to make sure users get a chance to explore the settings and tweak the style.
If you can help us out by fixing a soundness bug, you can really help the project move forward.

### bug (stylistic)

Besides breaking correctness another kind of bug is that the style of the output might not be what you expect.
Bugs like this are labelled as `bug (stylistic)`.
This includes cases where Fantomas breaks one of its own formatting rules or fails to respect one of its settings.

Again, here: scratch your own itch. If something bothers you, the best cure is to try a take a stab at it yourself.

## Adoption

The dream is that every F# developer can use Fantomas at any time.
This aspiration is an odyssey that might never be complete, but any step in that direction is most welcome.
Try introducing automatic formatting in your project, at work, or in an open-source project.
This tool will only improve by adoption.

### fsprojects

As Fantomas is part of the [F# Community Project Incubation Space](https://github.com/fsprojects/), it would be nice to see all the sibling projects formatted as well.  
We've put a lot of emphasis on [continued formatting](../end-users/FormattingCheck.md) using the `--check` flag. 