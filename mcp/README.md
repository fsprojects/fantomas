
# MCP

## Goal

Give contributors more tools to use AI to come up with fixes to the codebase.
Having a set of specific tools calls could improve the hallucinating and help an LLM avoid a bunch of BS.

## Tool calls

I think we will need to have a second fsi script which the mcp server can call.
Because we don't want a hard dependency on the source code.
Maybe the LLM made a change which leads to a broken code base.

### Parse

expose the parser as tool call.
Return both untyped F# AST and Oak?

### Format

expose the format call.
input should be source code, isFsi, settings (.editorconfig)
Include validation here as well?

### Rebuild

Produce a new binary of Fantomas.Core.
Should probably be called in most other tool calls.

### Trivia

Analyze which trivia was detected in which nodes.

### Define solver

Expose the define solver

### Style guide

Fetch both style guides and return in LLM friendly fashion.

### Test generation

Guide the LLM to generate tests.

## Prompts

We should probably also create a bunch of example prompt on how to ask the LLM to fix certain issues.

## Test issues

Try and solve https://github.com/fsprojects/fantomas/issues/3188
