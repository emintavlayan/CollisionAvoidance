namespace Shared

open System

/// Represents one detached todo item used by the starter SAFE sample.
type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) = {
        Id = Guid.NewGuid()
        Description = description
    }

/// Represents the SAFE remoting contract for the starter todo workflow.
type ITodosApi = {
    getTodos: unit -> Async<Todo list>
    addTodo: Todo -> Async<Todo list>
}

/// Represents the SAFE remoting contract for collision-run detail retrieval.
type ICollisionRunsApi = {
    getCollisionRunDetails: Guid -> Async<CollisionRunDetailsDto>
}
