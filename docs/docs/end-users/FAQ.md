---
category: End-users
categoryindex: 1
index: 1000
---
# FAQ

## Why the name "Fantomas"?
There are a few reasons to choose the name as such.
First, it starts with an "F" just like many other F# projects.
Second, Fantomas is my favourite character in the literature.
Finally, Fantomas has the same Greek root as "[phantom](https://en.wiktionary.org/wiki/phantom)"; coincidentally F# ASTs and formatting rules are so *mysterious* to be handled correctly.

## Why do I need prerelease?

Fantomas is both a tool and a library. Before Fantomas v5, the tool was called `fantomas-tool` and the library `Fantomas`.  
After some user feedback, we decided to rename `Fantomas` (the library) to `Fantomas.Core` and `fantomas-tool` to `fantomas`.

Once v5 is released, users will be able to install the tool using `dotnet tool install fantomas`.  
Because there is no stable release yet, `NuGet` will not resolve `fantomas` as a tool but as a regular library.
Thus, for the time being, you need to add `--prerelease` to resolve this.

At the time of writing we are in `beta` and consider **version five** to be **production ready**.

## Why exit code 99 for a failed format check?

No real reason, it was [suggested](https://github.com/fsprojects/fantomas/pull/655#discussion_r374849907) by the contributor [lpedrosa](https://github.com/lpedrosa).  
It also reminds us of a certain Jay-Z song 😉.

## Can I make a design suggestion?
TODO

## Is it safe to use the Alpha version of Fantomas?
TBD
<div class="d-flex justify-content-between my-4">
  <a href="./Benchmarks.html">Previous</a>
</div>
