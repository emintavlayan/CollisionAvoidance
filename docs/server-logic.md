# Server logic

The server layer is split between new collision-oriented helper modules and the original SAFE todo API entry point.

## Collision-oriented modules

### `CollisionAnalysis.fs`

- `createPendingSummary` creates a `CollisionRunSummaryDto` with `AnalysisPending` status at every level.
- The function preserves the beam and control-point structure from the input plan.
- No geometric collision calculation happens yet; this is only summary initialization.

### `CollisionSceneBuilder.fs`

- `buildScene` reshapes a `CollisionRunRequestDto` into a simpler `CollisionScene`.
- The scene currently just forwards `Body`, `Plan.Beams`, and `Accessories`.
- No filtering, coordinate transforms, or preprocessing happen yet.

### `AccessoryModels.fs`

- `enabledAccessories` filters accessory input down to `IsEnabled = true`.
- This is currently the only accessory-specific server logic.

### `RunStore.fs`

- `CollisionRunRecord` packages the original request together with an optional summary.
- `create` initializes a record with `Summary = None`.

## Live API entry point

### `Server.fs`

The running server is still the SAFE template todo API:

- `Storage.todos` is an in-memory `ResizeArray<Todo>`.
- `Storage.addTodo` validates and stores todos.
- `todosApi` exposes `getTodos` and `addTodo`.
- `webApp` and `app` publish that API through Saturn/SAFE.

This means the collision-analysis DTOs and helper modules are not yet exposed through the HTTP API.
