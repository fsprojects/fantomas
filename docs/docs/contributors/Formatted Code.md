---
category: Contributors
categoryindex: 2
index: 7
---
# Fantomas.Core overview (3)

After the `Context` travelled through the composed `CodePrinter` function, all events are captured.  
These can be converted to a string of formatted code.

<div class="mermaid text-center">
graph TD
    A[Transform source code to tree] --> B
    B[Traverse Oak to get formatted code] --> C[Formatted code]
    style C stroke:#338CBB,stroke-width:2px
 </div>

## Post processing

As a final step in the process, we validate the result of the code generation.  
We do this by parsing the existing code and investigating the fsharp diagnostics.
When there are any warnings or errors, we will throw an exception.
Some warnings are are allowed as they indicate problems that were most likely already present in the input code.
See `Validation.fs` for more details.

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
