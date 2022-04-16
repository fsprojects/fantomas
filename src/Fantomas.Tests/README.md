# Testing

## Dotnet test helpers

### Filtering

```bash
dotnet test --filter "testName"
```

Nunit doesn't like spaces in names. You can also use `Category` to group tests.

```fsharp
[<Test>]
[<Category("MyCategory")>]
let ``do the formatting`` () = ...
```

```bash
dotnet test --filter TestCategory=MyCategory
```

### Debugging

```bash
VSTEST_RUNNER_DEBUG=1 dotnet test
```
