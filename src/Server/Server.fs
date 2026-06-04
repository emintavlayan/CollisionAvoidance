module Server

open System.IO
open Microsoft.AspNetCore.DataProtection
open SAFE
open Saturn
open Shared
open Giraffe
open Microsoft.Extensions.DependencyInjection

module Storage =
    let todos: ResizeArray<Todo> =
        ResizeArray [
            Todo.create "Create new SAFE project"
            Todo.create "Write your app"
            Todo.create "Ship it!!!"
        ]

    let addTodo (todo: Todo) : Result<unit, string> =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

let todosApi ctx = {
    getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
    addTodo =
        fun (todo: Todo) -> async {
            return
                match Storage.addTodo todo with
                | Ok() -> Storage.todos |> List.ofSeq
                | Error e -> failwith e
        }
}

let collisionRunsApi ctx = {
    getCollisionRunDetails =
        fun runId -> async {
            match RunStore.tryGetRunDetails runId with
            | Some details -> return details
            | None -> return failwith $"Collision run {runId} was not found."
        }
}

let todoWebApp = Api.make todosApi
let collisionRunsWebApp = Api.make collisionRunsApi

let collisionRouter = router {
    post "/api/collision-runs" CollisionApi.createCollisionRunHandler
    getf "/api/collision-runs/%O/summary" CollisionApi.getCollisionRunSummaryHandler
    getf "/api/collision-runs/%O" CollisionApi.getCollisionRunDetailsHandler
    getf "/collision/%O" (fun _ ->
        let indexPath = Path.Combine(Directory.GetCurrentDirectory(), "public", "index.html")

        if File.Exists indexPath then
            htmlFile indexPath
        else
            htmlString "<html><body><h1>Collision run page</h1><p>The SAFE client bundle is not published in this server-only environment. Start the client dev server or publish the client assets to view the run summary UI.</p></body></html>")
}

let webApp =
    choose [
        collisionRouter
        collisionRunsWebApp
        todoWebApp
    ]

let app = application {
    service_config (fun services ->
        services
            .AddDataProtection()
            .PersistKeysToFileSystem(DirectoryInfo(Path.Combine(Directory.GetCurrentDirectory(), ".data-protection")))
            .SetApplicationName("CollisionAvoidance")
        |> ignore
        services
    )
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0
