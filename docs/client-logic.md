# Client logic

The client contains both the original todo page and several small modules for the collision-analysis UI model.

## Main Elmish flow

### `Index.fs`

This is still the active application flow.

- `Model` stores `Todos` and the current text `Input`.
- `Msg` supports input updates, loading todos, and saving todos.
- `init` starts by dispatching `LoadTodos(Start())`.
- `update` proxies todo fetch/save operations through `ITodosApi`.
- `view` renders the todo UI using Feliz components.

The page title says `CollisionAvoidance`, but the behavior is still the todo sample.

### `App.fs`

- Boots the Elmish program.
- Imports `index.css`.
- Enables console tracing in `DEBUG`.

## Collision-oriented client modules

### `CollisionRunPage.fs`

- `Model` tracks an optional run id and optional run summary.
- `init` starts with both values unset.

### `BeamSelection.fs`

- `init` creates a set containing every beam id from a plan.
- The default behavior is "all beams selected".

### `AccessorySelection.fs`

- `init` creates a set containing the ids of enabled accessories only.
- The default behavior follows `AccessoryModelDto.IsEnabled`.

### `ResultSummary.fs`

- `beamStatuses` maps a run summary into `(beamId, status)` pairs.

### `GeometryViewer.fs`

- `SceneModel` holds an optional body and a list of accessories.
- `empty` initializes the geometry viewer with no loaded scene.

## Current frontend state

The client has the beginnings of collision-specific state modules, but the main page has not yet been rewired to use them.
