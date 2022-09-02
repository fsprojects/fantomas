---
category: Contributors
categoryindex: 2
index: 9
---
# How can I contribute?

There are many ways to contribute to an open-source project. From liking a tweet to show some interest to solving a heavy handed coding problem.   
The most obvious thing where we can use some help is fixing bugs, but there are a lot of other things that most certainly would be welcome.

## Bug fixes

**The most welcome additions to the project are bug fixes**. The philosophy behind this is that everyone should be able to fix their own bug.
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
We've put a lot of emphasis on [continued formatting](../end-users/FormattingCheck.md) using the `--check` flag. Having that CI setup in place really brings it home.

### Big fish

For marketing purposes, it is also very interesting if a larger or well-known project is using Fantomas.  
We can put these on our [landing page](../../index.html#who-uses-fantomas) and that really sends a strong message.

### Any fish really

Regardless of size or type of project. Any project that checks Fantomas in their CI system [is most welcome](https://youtu.be/IQXby29_tVo).

## Sponsoring

Fantomas grew significantly as a result of its first sponsorship deal with [G-Research](https://www.gresearch.co.uk/).  
It would still be in the dark ages if it weren't for this support. For that we will forever be grateful.  
If you want to help increase adoption by providing financial support, you can reach out to [sponsoring@fantomas.io](mailto:sponsoring@fantomas.io).

## Keeping the grass green

There are also some smaller deeds that can benefit the codebase, which would be appreciated.

### Dead code

Here and there, there are piece in the code that are no longer being used. Ranging from parameters to complete functions.  
A PR that cleans up these things would be appreciated as well.

### Linting

Using F# Lint or other editor tooling, sometimes small improvements can be detected. Redundant parenthesis for example.  
Tweaks like this are nice.

## Understand how things work

One other thing that changes your perception of code all together is knowing how Fantomas does what it does.  
Having a sense of the inner workings of Fantomas can be beneficial in understanding how the output was achieved.  

It broadens your horizon in general, as it touched a lot of interesting concepts, and you start looking differently at your F# code.  
Your sentiment on what really matters might change, once you realized the level of complexity to get there.

## Documentation

Found a typo? Still confused about something? Do you have some knowledge that should totally be documented?  
Let us know! We really value any contribution to our documentation. The more knowledge is here, the brighter the future of the project.  
Please do no hesitate here 😊.

You can find some instructions on running the documentation locally in the [.README.md](https://github.com/fsprojects/fantomas/blob/master/docs/.README.md) file in the `docs` folder.  
The only prerequisite to run the docs, is having a recent local dotnet sdk.

## New releases

Testing out new releases is also a huge way to help us. Spotting on regressions early really helps to fix them early.

### Move to the latest

Always try and stay on the latest version of Fantomas for your day-to-day projects.  
Updating to the newer version that might only have a couple of fixes might seem insignificant, but it really helps.

### Try alpha's and beta's

Please give an alpha a spin if you are interested in submitting feedback for new development.  
Try both versions if you want to make sure everything still works for you.

Don't feel obliged to use an alpha/beta in your day-to-day flow, just try them to see if the potential next stable version will still work.  
We have **over 2000 unit tests**, that still doesn't tell us we will never break your specific code.

<fantomas-nav previous="./Conditional%20Compilation%20Directives.html"></fantomas-nav>