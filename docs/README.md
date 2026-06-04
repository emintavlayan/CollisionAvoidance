# Documentation

This folder breaks the current application logic into separate documents by layer.

- [Contracts](contracts.md): DTO boundary shared between ESAPI and SAFE
- [Collision analysis](collision-analysis.md): SAFE-side sampling and point-in-body workflow
- [ESAPI exporter](esapi-exporter.md): Eclipse-side extraction and submission boundary
- [Shared domain](shared-domain.md): DTOs and data contracts that move between client and server
- [Server logic](server-logic.md): how the server currently composes collision-related data and what is still template code
- [Client logic](client-logic.md): Elmish state and helper modules on the frontend
- [Testing](testing.md): test layout, framework choice, and current coverage

Read these documents as a description of the code that exists today, not as a statement that the full collision-analysis workflow is already wired together.
