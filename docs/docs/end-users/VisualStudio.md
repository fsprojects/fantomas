---
category: End-users
categoryindex: 1
index: 7
---
# Visual Studio
The F# Formatting extension sets up Fantomas as the default formatter for F# files, configurable from Visual Studio's options.  
Do note that the extension might not be up to date with the latest version of Fantomas.  

* [Visual Studio 2019](https://marketplace.visualstudio.com/items?itemName=asti.fantomas-vs)
* [Visual Studio 2022](https://marketplace.visualstudio.com/items?itemName=asti.fantomas-vs22)

Aditionally, you can set Fantomas as an external tool: 

1. Install [fantomas](https://www.nuget.org/packages/fantomas) locally with `dotnet tool install fantomas --prerelease` and configure it as an External Tool.
2. Open the external tool window by going to Tools -> Edit custom tools
3. Click Add and fill in the information  
4. Run fantomas inside Tools -> fantomas
  
<img src="{{root}}/images/vsmac-external-tool.png" alt="drawing" width="70%"/>

### Visual Studio for Mac
Similar to the above, you can set Fantomas as an external tool for Visual Studio for Mac.


<div class="d-flex justify-content-between my-4">
  <a href="./Rider.html">Previous</a>
  <a href="./VSCode.html">Next</a>
</div>