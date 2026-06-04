module Server.Tests

open Shared
open Server
open Xunit

[<Fact>]
let ``Adding a valid todo returns ok and stores the todo`` () =
    let validTodo = Todo.create "TODO"
    let initialCount = Storage.todos.Count

    let result = Storage.addTodo validTodo

    Assert.Equal<Result<unit, string>>(Ok(), result)
    Assert.Equal(initialCount + 1, Storage.todos.Count)
    Assert.Contains(validTodo, Storage.todos)

    let removed = Storage.todos.Remove validTodo

    Assert.True(removed)
    Assert.Equal(initialCount, Storage.todos.Count)
