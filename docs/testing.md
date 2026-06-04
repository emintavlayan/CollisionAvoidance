# Testing

The repository now uses xUnit for all F# test projects in `tests/`.

## Projects

- `tests/Shared`: shared-domain tests
- `tests/Server`: server-logic tests
- `tests/Client`: client-logic tests

## Test style

Tests use the standard F# xUnit shape:

```fsharp
[<Fact>]
let ``human readable description`` () =
    // assertions
```

This keeps test names readable in both source and test runners.

## Current coverage

The current suite is intentionally small and covers the logic that already exists:

- shared todo validation
- server todo storage behavior
- client update behavior when a save result is received

## Running tests

Use either of these:

```powershell
dotnet test Application.sln
dotnet run --project Build.fsproj -- WatchRunTests
```

The watch target now runs `dotnet watch test` for each test project instead of using the old browser-based Mocha harness.
