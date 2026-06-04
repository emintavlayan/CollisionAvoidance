open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let deployPath = Path.getFullName "deploy"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] clientPath // Delete *.fs.js files created by Fable
)

Target.create "RestoreClientDependencies" (fun _ -> run npm [ "ci" ] clientPath)

Target.create "Bundle" (fun _ ->
    [
        "server", dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] serverPath
        "client", dotnet [ "fable"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "build" ] clientPath
    ]
    |> runParallel)

Target.create "Azure" (fun _ ->
    let web = webApp {
        name "SAFE-App"
        operating_system OS.Linux
        runtime_stack (DotNet "8.0")
        zip_deploy "deploy"
    }

    let deployment = arm {
        location Location.WestEurope
        add_resource web
    }

    deployment |> Deploy.execute "SAFE-App" Deploy.NoParameters |> ignore)

Target.create "Build" (fun _ ->
    run dotnet [ "build"; "Application.sln" ] "."

    )

Target.create "Run" (fun _ ->
    [
        "server", dotnet [ "watch"; "run"; "--no-restore" ] serverPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] clientPath
    ]
    |> runParallel)

Target.create "RunTestsHeadless" (fun _ ->
    run
        dotnet
        [ "test"; "Application.sln"; "--no-build"; "--logger"; "console;verbosity=normal" ]
        "."
)

Target.create "WatchRunTests" (fun _ ->
    [
        "shared", dotnet [ "watch"; "test"; "tests/Shared/Shared.Tests.fsproj"; "--no-restore" ] "."
        "server", dotnet [ "watch"; "test"; "tests/Server/Server.Tests.fsproj"; "--no-restore" ] "."
        "client", dotnet [ "watch"; "test"; "tests/Client/Client.Tests.fsproj"; "--no-restore" ] "."
    ]
    |> runParallel)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [
    "Clean" ==> "RestoreClientDependencies" ==> "Bundle" ==> "Azure"
    "Clean" ==> "RestoreClientDependencies" ==> "Build" ==> "Run"

    "RestoreClientDependencies" ==> "Build" ==> "RunTestsHeadless"
    "RestoreClientDependencies" ==> "Build" ==> "WatchRunTests"
]

[<EntryPoint>]
let main args = runOrDefault args
