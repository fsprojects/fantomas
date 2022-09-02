---
category: End-users
categoryindex: 1
index: 7
---
# JetBrains Rider
The resharper-fsharp uses fantomas under the hood to format the source code. No need for any additional plugins.  
  
From Rider 2022.2 onwards, Rider can detect your `dotnet` fantomas installation, either globally or locally.  
Install fantomas locally with `dotnet tool install fantomas --prerelease`   
Manually set your fantomas version by going to Preferences -> F#, under the fantomas tab.
<img class="mt-2" src="{{root}}/images/rider-fantomas.png" alt="drawing" width="70%"/>
<div class="d-flex justify-content-between my-4">
  <a href="./GitHooks.html">Previous</a>
  <a href="./VisualStudio.html">Next</a>
</div>