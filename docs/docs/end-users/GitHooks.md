---
category: End-users
categoryindex: 1
index: 6
---
# Git hooks
## A git pre-commit hook sample

A very elegant and transparent way to use Fantomas is including it in a pre-commit git hook, by creating a `.git/hooks/pre-commit` file with:
##### Using fantomas globally
```
#!/bin/sh
git diff --cached --name-only --diff-filter=ACM -z | xargs -0 $HOME/.dotnet/tools/fantomas
git diff --cached --name-only --diff-filter=ACM -z | xargs -0 git add
```

This script assumes you have installed Fantomas globally as a [dotnet tool](https://www.nuget.org/packages/fantomas/)
##### Using fantomas locally
```
#!/bin/sh
git diff --cached --name-only --diff-filter=ACM -z | xargs -0 dotnet fantomas 
git diff --cached --name-only --diff-filter=ACM -z | xargs -0 git add
```

**Please use with caution** as [Fantomas is not without bugs](https://github.com/fsprojects/fantomas/issues?q=is%3Aissue+is%3Aopen+label%3A%22bug+%28soundness%29%22).

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
