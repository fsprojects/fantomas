# AGENTS.md - AI Agent Guide for Fantomas

This guide is specifically designed for AI agents working on the Fantomas F# code formatter project. It provides essential information about project structure, development workflow, and best practices.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Project Structure](#project-structure)
3. [Development Workflow](#development-workflow)
4. [Core Architecture](#core-architecture)
5. [Testing Guidelines](#testing-guidelines)
6. [Common Tasks](#common-tasks)
7. [MCP Tools](#mcp-tools)
8. [Configuration](#configuration)
9. [Error Handling](#error-handling)
10. [Code Style](#code-style)
11. [Troubleshooting](#troubleshooting)
12. [Resources](#resources)

## Project Overview

Fantomas is an F# code formatter that transforms F# source code into a standardized format. The project follows a two-phase approach:

1. **Transform**: Parse F# source code into a custom tree model (Oak)
2. **Traverse**: Walk the tree to generate formatted code

### Key Principles

- **Valid Code Only**: Fantomas requires valid F# source code to format
- **Test-Driven Development**: Always write tests before implementing fixes
- **Minimal Changes**: Make the smallest possible changes to achieve the goal
- **Consistency**: Follow existing code patterns and conventions

### Quick Start Commands

```bash
# Most common commands for LLMs
dotnet build                            # Quick build check (use during development)
dotnet test --filter "test-name"        # Run specific test
dotnet fsi build.fsx -p FormatChanged   # Format changes
dotnet fsi build.fsx                    # Full build and test (use for final verification)
dotnet fsi build.fsx -p FormatAll       # Format all files
dotnet fsi build.fsx -- -p Analyze      # Run code analyzers
```

## Project Structure

```
fantomas/
├── src/
│   ├── Fantomas.Core/           # Core formatting engine
│   │   ├── CodePrinter.fs       # Main code generation logic
│   │   ├── SyntaxOak.fs         # Custom tree model (also includes types of Trivia)
│   │   └── Context.fs           # Formatting context and events (includes lots of helpers)
│   ├── Fantomas.Core.Tests/     # Unit tests
│   │   ├── ModuleTests.fs       # Module-related tests
│   │   └── TestHelpers.fs       # Test utilities
│   ├── Fantomas.FCS/            # F# Compiler Service integration
│   └── Fantomas/                # CLI interface
├── docs/                        # Documentation
├── build.fsx                    # Build script
└── global.json                  # .NET SDK version
```

### Key Files for AI Agents

### `src/Fantomas.Core/CodePrinter.fs`

- Main formatting logic
- Constructs pipeline for `Context` instance to collect `WriterEvents` based on `Oak` model
- `genNode` is common helper for `Trivia` of `Node` to be processed
- **Critical**: Changes to `genNode` impact the entire file - scope logic closer to where problems reside instead
- Careful with edits to auxiliary functions, changing these can be very impactful, consider local edits instead

### `src/Fantomas.Core/SyntaxOak.fs`

- Custom tree model definitions
- `Node` can hold `Trivia` where nodes from `Fantomas.FCS.Syntax.ParsedInput` cannot
- `TriviaContent` is union types for the trivia types (comments, newlines, directives, cursor)

### `src/Fantomas.Core/Trivia.fs`

- Trivia assigment
- Different algoritm based `TriviaContent`
- Assigment is best effort approach, hard to perfect

### `src/Fantomas.Core/Context.fs`

- `WriterEvents` models the formatted result code without committing yet.
- Often a simple code formatting path is tried, to only be reverted when the code does not fit a given threshold

### `src/Fantomas.Core.Tests/*Tests.fs`

- Test files have a fixed format
- Input and output in test should be valid F# code
- Try and change as little as possible config values

## Development Workflow

### 1. Setup

```bash
# Clone and setup
git clone <your-fork>
cd fantomas
dotnet tool restore
dotnet restore

# Build and test
dotnet fsi build.fsx
```

### 2. Making Changes

1. **Write Test First**: Create a failing test that demonstrates the issue
2. **Implement Fix**: Make minimal changes to fix the issue
3. **Verify**: Run tests to ensure fix works
4. **Format**: Format your changes using the build script

### 3. Testing

```bash
# Quick build check during development
dotnet build

# Run specific test
dotnet test src/Fantomas.Core.Tests/ --filter "test-name"

# Run tests in release mode (recommended to avoid stack overflows on Mac)
dotnet test src/Fantomas.Core.Tests/ --filter "test-name" -c Release

# Format changed files
dotnet fsi build.fsx -p FormatChanged

# Full build and test (use for final verification)
dotnet fsi build.fsx
```

### 4. Build Commands

```bash
# Full build with tests
dotnet fsi build.fsx

# Format all files
dotnet fsi build.fsx -p FormatAll

# Format only changed files
dotnet fsi build.fsx -p FormatChanged

# Setup git hooks
dotnet fsi build.fsx -p EnsureRepoConfig

# Run code analyzers
dotnet fsi build.fsx -- -p Analyze
```

## Core Architecture

### Two-Phase Processing

#### Phase 1: Transform to Oak
1. **Parse**: Use F# compiler to parse source into untyped AST
2. **Transform**: Map AST to custom Oak tree model
3. **Collect Trivia**: Add comments, directives, and blank lines

#### Phase 2: Traverse to Code
1. **Generate Events**: Walk Oak tree to create WriterEvents
2. **Apply Context**: Use Context to manage indentation and formatting
3. **Output**: Convert events to formatted source code

### Key Components

#### Oak Tree Model
- **NodeBase**: Base class for all nodes with trivia support
- **SingleTextNode**: Represents text tokens
- **IdentListNode**: Represents identifiers with dots
- **ModuleOrNamespaceNode**: Represents module/namespace declarations

#### Context System
- **WriterEvents**: Capture formatting actions (indent, newline, text)
- **WriterModel**: Track current state (column, indentation level)
- **Config**: Formatting configuration options

#### Indentation System
- **Indentation Events**: `IndentBy`, `UnIndentBy`, `SetIndent`, `RestoreIndent`
- **Column Tracking**: `AtColumn` helps maintain alignment at specific positions
- **Critical Rule**: Indentation only takes effect after `WriteLine`/`WriteLineBecauseOfTrivia` events
- **Common Pattern**: `indent` → `sepNln` → content → `unindent`
- **Helper Functions**: `indentSepNlnUnindent`, `atCurrentColumn`, `atCurrentColumnIndent`

#### Trivia System
- **ContentBefore/ContentAfter**: Attach comments and directives to nodes
- **Directive**: Conditional compilation directives (#if, #endif)
- **Comment**: Line and block comments
- **Newline**: Blank lines and spacing

### Common Patterns

#### Adding New Oak Node Types
1. Define the node type in `SyntaxOak.fs`
2. Add transformation logic in `ASTTransformer.fs`
3. Add formatting logic in `CodePrinter.fs`
4. Update tests in appropriate `*Tests.fs` file

#### CodePrinter Patterns
- Use `genNode` for common trivia handling
- Use `genTriviaFor` for specific trivia types
- Use `dumpAndContinue` for debugging context state
- Prefer local helper functions over modifying global ones

#### Trivia Assignment Patterns
- Use `Trivia.enrichTree` to attach trivia to nodes
- Check `hasDirectivesInContentBefore` for conditional compilation
- Use `insertCursor` for cursor position handling

## Testing Guidelines

### Test Structure

```fsharp
[<Test>]
let ``descriptive test name, issue-number`` () =
    formatSourceString """
    // Input code here
    """ config
    |> prepend newline
    |> should equal """
    // Expected output here
    """
```

### Test Naming Convention

- Start with lowercase letter
- Use descriptive names
- Include issue number for bug fixes: `"fix description, 1234"`
- Use backticks for multi-word names

### Test Categories

- We organize the tests in `Fantomas.Core.Tests` according to syntax constructs:
  - **ModuleTests.fs**: Module and namespace formatting
  - **TypeTests.fs**: Type definition formatting
  - **ExpressionTests.fs**: Expression formatting
  - **PatternTests.fs**: Pattern matching formatting
- `src/Fantomas.Core.Tests/CodePrinterHelperFunctionsTests.fs` can be insightful to understand the `Context` pipeline setup.

### Writing Tests

1. **Reproduce Issue**: Create test that shows the problem
2. **Set Expectations**: Define expected output
3. **Verify Fix**: Ensure test passes after implementation
4. **Add Variations**: Test edge cases and similar scenarios

## Common Tasks

### Fixing Formatting Issues

1. **Identify Problem**: Understand what's wrong with current formatting
2. **Locate Code**: Find relevant code in `CodePrinter.fs`
3. **Write Test**: Create failing test for the issue
4. **Implement Fix**: Modify formatting logic
5. **Verify**: Run tests and check for regressions

### Adding New Features

1. **Understand Requirement**: Clarify what needs to be formatted
2. **Find Similar Code**: Look for existing patterns in CodePrinter
3. **Extend Oak Model**: Add new node types if needed
4. **Implement Logic**: Add formatting logic
5. **Add Tests**: Cover all code paths and edge cases

### Working with Conditional Directives

- Directives are stored as trivia on nodes
- Use `hasDirectivesInContentBefore` to detect directives
- Apply special formatting when directives are present
- Test with different define combinations

### Debugging CodePrinter

- Use `dumpAndContinue` to inspect Context during traversal
- Breakpoints in CodePrinter show function composition, not execution
- Check WriterEvents to understand formatting decisions
- Use MCP tools to test formatting in real-time

### Understanding Indentation

Indentation in Fantomas is **deferred** - it only takes effect after newline events:

```fsharp
// This pattern is very common in CodePrinter:
indent +> sepNln +> content +> unindent

// Or using the helper:
indentSepNlnUnindent content
```

**Key Points:**
- `indent` adds an `IndentBy` event to the context
- The actual indentation only applies when `WriteLine`/`WriteLineBecauseOfTrivia` events are processed
- `WriterModel.update` in `Context.fs` handles this by setting `Indent = max m.Indent m.AtColumn` on newlines
- Always pair `indent` with `unindent` to avoid indentation drift
- Use `atCurrentColumn` and `atCurrentColumnIndent` for fixed column positioning

**Example from CodePrinterHelperFunctionsTests.fs:**
```fsharp
let g = !-"first line" +> indent +> sepNln +> !-"second line" +> unindent
// Result: "first line\n    second line"
```

### Understanding genNode and Trivia Processing

The `genNode` function is crucial for understanding how indentation interacts with trivia:

```fsharp
let genNode<'n when 'n :> Node> (n: 'n) (f: Context -> Context) =
    enterNode n +> recordCursorNode f n +> leaveNode n

let enterNode<'n when 'n :> Node> (n: 'n) =
    col sepNone n.ContentBefore (genTrivia n)
```

**Critical Understanding:**
- `genNode` processes `ContentBefore` trivia first, then runs the function, then `ContentAfter` trivia
- `genTrivia` handles directives and comments, often emitting `sepNlnForTrivia` events
- **Trivia processing can override indentation**: When `genTrivia` processes directives, it may emit newline events that apply indentation before your intended content
- This is why `indentSepNlnUnindent` patterns can fail when nodes have trivia - the trivia emits newlines that consume the indentation

**Common Issue Pattern:**
```fsharp
// This fails when node has ContentBefore trivia:
indentSepNlnUnindent (genIdentListNode node)
// Because genIdentListNode calls genNode, which processes trivia first
// The trivia emits newlines that apply indentation before the module name
```

**Solution Approaches:**
1. **Bypass genNode**: Write content directly without trivia processing
2. **Handle trivia separately**: Process trivia before applying indentation
3. **Use different indentation strategy**: Apply indentation at a different level in the tree

## MCP Tools

### Fantomas Format Code Tool

The MCP Fantomas tool provides real-time formatting testing and debugging capabilities. It's essential for understanding how code transformations work.

#### Usage

```bash
# Tool configuration (in ~/.cursor/mcp.json)
"Fantomas": {
  "type": "stdio",
  "command": "dotnet",
  "args": ["fsi", "/path/to/fantomas/mcp/server.fsx"]
}
```

#### Expected Usage

1. **Input Requirements**: The tool expects valid F# code as input
   - Invalid F# code will result in parse errors and limited debugging information
   - Use the tool with problematic but syntactically correct F# code

2. **Tool Behavior**:
   - Automatically builds the local Fantomas codebase before formatting
   - Reports detailed events during the formatting process
   - Shows transformation from F# source → Untyped AST → Syntax Oak → WriterEvents
   - Validates that formatted output is still valid F# code

3. **Debugging Workflow**:
   ```fsharp
   // Start with problematic code
   let problematicCode = """
   module Test
   let x=1+2
   """
   
   // Use MCP tool to see transformation events
   // Analyze WriterEvents to understand formatting decisions
   // Identify where formatting logic needs adjustment
   ```

4. **Integration with Development**:
   - Use before writing tests to understand expected behavior
   - Use to debug existing formatting issues
   - Use to verify fixes work as expected

## Configuration

### Finding Configuration Options

Configuration is primarily handled through `FormatConfig` in the codebase:

```fsharp
// Common configuration options
type FormatConfig = {
    IndentSize: int
    MaxLineLength: int
    // ... other options
}
```

### Configuration Best Practices

1. **Minimal Changes**: Try to change as few config values as possible
2. **Test Impact**: Always test configuration changes with existing tests
3. **Documentation**: Document why specific config values are needed
4. **Default Behavior**: Prefer using default configuration when possible

### Common Configuration Patterns

```fsharp
// Use default config for most tests
let config = FormatConfig.Default

// Only modify specific settings when necessary
let customConfig = 
    { FormatConfig.Default with 
        IndentSize = 2
        MaxLineLength = 120 }
```

## Error Handling

### Parse Errors

Fantomas requires valid F# code to format. Parse errors are handled in `CodeFormatterImpl.fs`:

```fsharp
// Parse errors raise ParseException
if not errors.IsEmpty then
    raise (ParseException baseDiagnostics)
```

#### Common Parse Error Scenarios

1. **Syntax Errors**: Invalid F# syntax will cause parse failures
2. **Missing Dependencies**: Unresolved type references
3. **Conditional Compilation**: Issues with `#if` directives

#### Handling Parse Errors

- **For Testing**: Ensure test input is valid F# code
- **For Development**: Fix syntax errors before testing formatting
- **For MCP Tool**: Use valid F# code to get meaningful debugging output

### Format Errors

#### "The formatted result is not valid F# code"
- Check if formatting logic produces syntactically correct output
- Verify indentation and spacing are correct
- Test with F# compiler to ensure validity

#### WriterEvent Errors
- Check Context state during formatting
- Use `dumpAndContinue` to inspect intermediate states
- Verify WriterEvents produce valid code sequences

## Code Style

### F# Conventions Used in Fantomas

1. **Naming**:
   - Use camelCase for functions and variables
   - Use PascalCase for types and modules
   - Use descriptive names that explain intent

2. **Formatting**:
   - Follow Fantomas' own formatting rules
   - Use consistent indentation (4 spaces)
   - Prefer composition over complex expressions

3. **Patterns**:
   - Use discriminated unions for tree nodes
   - Use active patterns for AST matching
   - Prefer immutable data structures

4. **Comments**:
   - Use `//` for single-line comments
   - Use `(* *)` for multi-line comments
   - Document complex algorithms and decisions

### File Organization

- **One type per file** when possible
- **Logical grouping** of related functions
- **Clear separation** between public and internal APIs

## Troubleshooting

### Common Issues

#### "The formatted result is not valid F# code"
- Check if the fix produces syntactically correct F# code
- Verify indentation and spacing
- Test with F# compiler to ensure validity

#### Tests Failing
- Ensure test expectations match actual output
- Check for whitespace differences
- Verify test is using correct configuration

#### Build Errors
- Run `dotnet clean` and rebuild
- Check for syntax errors in your changes
- Ensure all dependencies are restored

#### Stack Overflow on Mac
- Stack overflows can occur in debug mode on macOS
- Use `-c Release` flag when running tests: `dotnet test -c Release`
- This is a known issue with F# compilation in debug mode on macOS

### Debugging Tips

1. **Use MCP Tools**: Test formatting with `mcp_Fantomas_format_code`
2. **Check Events**: Examine WriterEvents to understand formatting
3. **Compare Outputs**: Use diff tools to compare expected vs actual
4. **Isolate Changes**: Make minimal changes and test incrementally

## Resources

### Documentation
- [Getting Started](https://fsprojects.github.io/fantomas/docs/contributors/Getting%20Started.html)
- [Core Architecture](https://fsprojects.github.io/fantomas/docs/contributors/Transforming.html)
- [CodePrinter Guide](https://fsprojects.github.io/fantomas/docs/contributors/Traverse.html)
- [Pull Request Guidelines](https://fsprojects.github.io/fantomas/docs/contributors/Pull%20request%20ground%20rules.html)

### Key Concepts
- [Conditional Compilation Directives](https://fsprojects.github.io/fantomas/docs/contributors/Conditional%20Compilation%20Directives.html)
- [Trivia Handling](https://fsprojects.github.io/fantomas/docs/contributors/The%20Missing%20Comment.html)
- [Multiple Defines](https://fsprojects.github.io/fantomas/docs/contributors/Multiple%20Times.html)

### Tools
- **MCP Fantomas**: Real-time formatting testing and debugging
- **Build Script**: `dotnet fsi build.fsx` for all operations
- **Test Framework**: NUnit with FsUnit assertions
- **Git Hooks**: Automatic formatting on commit

### Best Practices

1. **Always Test First**: Write failing test before implementing fix
2. **Minimal Changes**: Make smallest possible change to fix issue
3. **Follow Patterns**: Use existing code patterns and conventions
4. **Document Changes**: Add meaningful commit messages and PR descriptions
5. **Verify Validity**: Ensure formatted code is valid F# code
6. **Check Regressions**: Run full test suite to catch unintended changes

### Example Workflow

```bash
# 1. Setup
git checkout -b fix-3188
dotnet build

# 2. Write test
# Add test to ModuleTests.fs

# 3. Implement fix
# Modify CodePrinter.fs

# 4. Test and verify
dotnet test src/Fantomas.Core.Tests/ --filter "Name~3188"
dotnet build

# 5. Format changes
dotnet fsi build.fsx -p FormatChanged

# 6. Final verification
dotnet fsi build.fsx
```

This guide should help AI agents understand the Fantomas project structure and contribute effectively to the codebase.

