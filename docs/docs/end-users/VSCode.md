---
category: End-users
categoryindex: 1
index: 8
---
# Visual Studio Code
The recommended way to use Fantomas in Visual Studio Code is by using the [Ionide plugin](http://ionide.io/). Fantomas is integrated in [FSAutoComplete](https://github.com/fsharp/FsAutoComplete/) which is the language server used by Ionide.  
Make sure Ionide is set to the default formatter inside `settings.json` :

```
  "[fsharp]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "Ionide.Ionide-fsharp"
  }
```

### Fantomas version detection used by Fantomas.Client

*starting version 4.6*

Fantomas version detection will try and find a compatible version in the following order:

1. The version of Fantomas used by your local project. This is the fantomas version displayed when you run `dotnet tool list` inside the project folder.  
2. Your global Fantomas version. If fantomas was installed with `dotnet tool install fantomas -g`. You can see your global installations with `dotnet tool list -g`.  
3. Executable named `fantomas` found in your PATH.

<fantomas-nav previous="./VisualStudio.html" next="./FAQ.html"></fantomas-nav>
