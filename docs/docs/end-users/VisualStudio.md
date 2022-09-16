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

### Visual Studio for Mac

1. Install [fantomas](https://www.nuget.org/packages/fantomas) locally with `dotnet tool install fantomas` and configure it as an External Tool.
2. Open the external tool window by going to Tools -> Edit custom tools
3. Click Add and fill in the information  
4. Run fantomas inside Tools -> fantomas
  
<img src="{{root}}/images/vsmac-external-tool.png" alt="drawing" width="70%"/>

<fantomas-nav previous="./Rider.html" next="./VSCode.html"></fantomas-nav>
