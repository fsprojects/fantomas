---
category: Contributors
categoryindex: 2
index: 7
---
# Fantomas.Core overview (3)

After the `Context` travelled throught the composed `CodePrinter` function, all events are captured.  
These can be converted to a string of formatted code.

<div class="mermaid text-center">
graph TD
    A[Prepare Context] --> B
    B[Print AST with Context] --> C[Formatted code]
    style C stroke:#338CBB,stroke-width:2px
 </div>

## Post processing

As a final step in the process, we validate the result of the code generation.  
We do this by parsing the existing code and investigating the fsharp diagnostics.
When there are any warnings or errors, we will throw an exception.
Some warnings are are allowed as they indicate problems that were most likely already present in the input code.
See `Validation.fs` for more details.

<div class="d-flex justify-content-between my-4">
  <a href="./Print%20AST%20with%20Context.html">Previous</a>
  <a href="./Conditional%20Compilation%20Directives.html">Next</a> 
</div>