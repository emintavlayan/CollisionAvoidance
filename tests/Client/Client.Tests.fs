module Client.Tests

open Index
open Shared
open SAFE
open Xunit

[<Fact>]
let ``Saving a todo result updates the loaded list`` () =
    let newTodo = Todo.create "new todo"
    let model, _ = initForPath "/"
    let model, _ = update (SaveTodo(Finished [ newTodo ])) model

    Assert.Equal(1, model.Todos |> RemoteData.map _.Length |> RemoteData.defaultValue 0)

    Assert.Equal(
        newTodo,
        model.Todos
        |> RemoteData.map List.head
        |> RemoteData.defaultValue (Todo.create "")
    )

[<Fact>]
let ``A collision route path is parsed into a collision page`` () =
    let runId = System.Guid.NewGuid()
    let model, cmd = initForPath $"/collision/{runId}"

    match model.Page with
    | CollisionPage pageModel ->
        Assert.Equal(runId, pageModel.RunId)
        Assert.NotNull(cmd)
    | TodoPage ->
        failwith "Expected collision route initialization to select the collision page."
