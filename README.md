# CollisionAvoidance

CollisionAvoidance is a SAFE-stack F# application that is being shaped into a collision-analysis workflow for treatment plans, body geometry, and enabled accessories.

The codebase currently has two layers of logic:

- A collision-analysis domain model in `src/Shared` and some server/client helper modules built around that model.
- The original SAFE template todo flow, which is still the live API and UI path in `src/Server/Server.fs` and `src/Client/Index.fs`.

## Repository layout

- `src/Shared`: shared DTOs for plans, geometry, accessories, and collision results
- `src/Server`: server-side composition and collision-analysis scaffolding
- `src/Client`: Elmish/Feliz client logic and view helpers
- `tests`: xUnit test projects for shared, server, and client logic
- `docs`: project documentation split by concern

## Getting started

Prerequisites:

- .NET 8 SDK
- Node.js 18+ and npm 9+ for the client app

Useful commands:

```powershell
dotnet run --project Build.fsproj -- Run
dotnet test Application.sln
dotnet run --project Build.fsproj -- WatchRunTests
```

The default SAFE development setup serves:

- Client: `http://localhost:8080`
- Server proxy: `http://localhost:5000`

## Documentation

- [Docs index](docs/README.md)
- [Shared domain](docs/shared-domain.md)
- [Server logic](docs/server-logic.md)
- [Client logic](docs/client-logic.md)
- [Testing](docs/testing.md)

## Current status

The collision domain model is already defined, but the end-to-end collision run flow is still a scaffold. The server entry point and the main client page still expose the starter todo example, while the newer collision modules exist as isolated building blocks.
