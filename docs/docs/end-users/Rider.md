---
category: End-users
categoryindex: 1
index: 7
---
# JetBrains Rider
The resharper-fsharp uses Fantomas under the hood to format the source code. No need for any additional plugins.  
  
From Rider 2022.2 onwards, Rider can detect your `dotnet` Fantomas installation, either globally or locally.

Install Fantomas locally with:
```
dotnet tool install fantomas
```

<img class="mt-2" src="{{root}}/images/rider-fantomas.png" alt="drawing" width="70%"/>

Prior to Rider 2022.3 it did not [respect](https://youtrack.jetbrains.com/issue/RIDER-83997/Rider-doesnt-respect-Fantomas-default-settings-not-explicitly-set-in-editorconfig) the default settings of Fantomas.
If you are stuck on version 2022.2 (or earlier) consider adding the default settings of Fantomas to your `.editorconfig` file.

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
