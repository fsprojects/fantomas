# AGENTS.md - AI Agent Guide for Fantomas

This guide is specifically designed for AI agents working on the Fantomas F# code formatter project. It provides essential information about project structure, development workflow, and best practices.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Project Structure](#project-structure)
3. [Development Workflow](#development-workflow)
4. [Core Architecture](#core-architecture)
5. [Testing Guidelines](#testing-guidelines)
6. [Common Tasks](#common-tasks)
7. [Troubleshooting](#troubleshooting)
8. [Resources](#resources)

## Project Overview

Fantomas is an F# code formatter that transforms F# source code into a standardized format. The project follows a two-phase approach:

1. **Transform**: Parse F# source code into a custom tree model (Oak)
2. **Traverse**: Walk the tree to generate formatted code

### Key Principles

- **Valid Code Only**: Fantomas requires valid F# source code to format
- **Test-Driven Development**: Always write tests before implementing fixes
- **Minimal Changes**: Make the smallest possible changes to achieve the goal
- **Consistency**: Follow existing code patterns and conventions

## Project Structure

```
fantomas/
├── src/
│   ├── Fantomas.Core/           # Core formatting engine
│   │   ├── CodePrinter.fs       # Main code generation logic
│   │   ├── SyntaxOak.fs         # Custom tree model
│   │   └── Context.fs           # Formatting context and events
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

- **`src/Fantomas.Core/CodePrinter.fs`**: Main formatting logic
- **`src/Fantomas.Core/SyntaxOak.fs`**: Tree model definitions
- **`src/Fantomas.Core.Tests/ModuleTests.fs`**: Module formatting tests
- **`src/Fantomas.Core.Tests/TestHelpers.fs`**: Test utilities

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
# Run all tests
dotnet fsi build.fsx

# Run specific test
dotnet test src/Fantomas.Core.Tests/ --filter "test-name"

# Format changed files
dotnet fsi build.fsx -p FormatChanged
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

#### Trivia System
- **ContentBefore/ContentAfter**: Attach comments and directives to nodes
- **Directive**: Conditional compilation directives (#if, #endif)
- **Comment**: Line and block comments
- **Newline**: Blank lines and spacing

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

- **ModuleTests.fs**: Module and namespace formatting
- **TypeTests.fs**: Type definition formatting
- **ExpressionTests.fs**: Expression formatting
- **PatternTests.fs**: Pattern matching formatting

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
- **MCP Fantomas**: Real-time formatting testing
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
dotnet fsi build.fsx

# 2. Write test
# Add test to ModuleTests.fs

# 3. Implement fix
# Modify CodePrinter.fs

# 4. Test and verify
dotnet test src/Fantomas.Core.Tests/ --filter "3188"
dotnet fsi build.fsx

# 5. Format changes
dotnet fsi build.fsx -p FormatChanged

# 6. Final verification
dotnet fsi build.fsx
```

This guide should help AI agents understand the Fantomas project structure and contribute effectively to the codebase.

