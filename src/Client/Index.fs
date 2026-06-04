module Index

open System
open Elmish
open SAFE
open Shared

type Page =
    | TodoPage
    | CollisionPage of CollisionRunPage.Model

type Model = {
    Page: Page
    Todos: RemoteData<Todo list>
    Input: string
}

type Msg =
    | SetInput of string
    | LoadTodos of ApiCall<unit, Todo list>
    | SaveTodo of ApiCall<string, Todo list>
    | LoadCollisionRun of ApiCall<Guid, Result<CollisionRunDetailsDto, string>>

let todosApi = Api.makeProxy<ITodosApi> ()
let collisionRunsApi = Api.makeProxy<ICollisionRunsApi> ()

#if FABLE_COMPILER
let getCurrentPath () = Browser.Dom.window.location.pathname
#else
let getCurrentPath () = "/"
#endif

let initForPath path =
    match CollisionRunPage.tryParseRunIdFromPath path with
    | Some runId ->
        let initialModel =
            {
                Page = CollisionPage(CollisionRunPage.init runId)
                Todos = NotStarted
                Input = ""
            }

        initialModel, LoadCollisionRun(Start runId) |> Cmd.ofMsg
    | None ->
        let initialModel =
            {
                Page = TodoPage
                Todos = NotStarted
                Input = ""
            }

        initialModel, LoadTodos(Start()) |> Cmd.ofMsg

let init () =
    initForPath (getCurrentPath ())

let update msg model =
    match msg with
    | SetInput value -> { model with Input = value }, Cmd.none
    | LoadTodos msg ->
        match msg with
        | Start() ->
            let loadTodosCmd = Cmd.OfAsync.perform todosApi.getTodos () (Finished >> LoadTodos)

            { model with Todos = model.Todos.StartLoading() }, loadTodosCmd
        | Finished todos -> { model with Todos = Loaded todos }, Cmd.none
    | SaveTodo msg ->
        match msg with
        | Start todoText ->
            let saveTodoCmd =
                let todo = Todo.create todoText
                Cmd.OfAsync.perform todosApi.addTodo todo (Finished >> SaveTodo)

            { model with Input = "" }, saveTodoCmd
        | Finished todos ->
            {
                model with
                    Todos = RemoteData.Loaded todos
            },
            Cmd.none
    | LoadCollisionRun msg ->
        match model.Page, msg with
        | CollisionPage collisionPage, Start runId ->
            let loadCollisionRunCmd =
                Cmd.OfAsync.either
                    collisionRunsApi.getCollisionRunDetails
                    runId
                    (Ok >> Finished >> LoadCollisionRun)
                    (fun error -> Error error.Message |> Finished |> LoadCollisionRun)

            {
                model with
                    Page =
                        CollisionPage {
                            collisionPage with
                                Details = collisionPage.Details.StartLoading()
                                ErrorMessage = None
                        }
            },
            loadCollisionRunCmd
        | CollisionPage collisionPage, Finished result ->
            match result with
            | Ok details ->
                {
                    model with
                        Page =
                            CollisionPage {
                                collisionPage with
                                    Details = Loaded details
                                    ErrorMessage = None
                            }
                },
                Cmd.none
            | Error errorMessage ->
                {
                    model with
                        Page =
                            CollisionPage {
                                collisionPage with
                                    Details = NotStarted
                                    ErrorMessage = Some errorMessage
                            }
                },
                Cmd.none
        | _ ->
            model, Cmd.none

open Feliz

module ViewComponents =
    let summaryValue (label: string) (value: string) =
        Html.div [
            prop.className "rounded-lg bg-white/20 px-4 py-3 border border-white/30"
            prop.children [
                Html.p [ prop.className "text-xs uppercase tracking-wide text-slate-700"; prop.text label ]
                Html.p [ prop.className "text-lg font-semibold text-slate-900"; prop.text value ]
            ]
        ]

    let todoAction model dispatch =
        Html.div [
            prop.className "flex flex-col sm:flex-row mt-4 gap-4"
            prop.children [
                Html.input [
                    prop.className
                        "shadow appearance-none border rounded w-full py-2 px-3 outline-none focus:ring-2 ring-teal-300 text-grey-darker text-sm sm:text-base"
                    prop.value model.Input
                    prop.placeholder "What needs to be done?"
                    prop.autoFocus true
                    prop.onChange (SetInput >> dispatch)
                    prop.onKeyPress (fun ev ->
                        if ev.key = "Enter" then
                            dispatch (SaveTodo(Start model.Input)))
                ]
                Html.button [
                    prop.className
                        "flex-no-shrink p-2 px-12 rounded bg-teal-600 outline-none focus:ring-2 ring-teal-300 font-bold text-white hover:bg-teal disabled:opacity-30 disabled:cursor-not-allowed text-sm sm:text-base"
                    prop.disabled (Todo.isValid model.Input |> not)
                    prop.onClick (fun _ -> dispatch (SaveTodo(Start model.Input)))
                    prop.text "Add"
                ]
            ]
        ]

    let todoList model dispatch =
        Html.div [
            prop.className "rounded-md p-2 sm:p-4 w-full"
            prop.children [
                Html.ol [
                    prop.className "list-decimal ml-4 sm:ml-6"
                    prop.children [
                        match model.Todos with
                        | NotStarted -> Html.text "Not Started."
                        | Loading None -> Html.text "Loading..."
                        | Loading (Some todos)
                        | Loaded todos ->
                            for todo in todos do
                                Html.li [
                                    prop.className "my-1 text-black text-base sm:text-lg break-words"
                                    prop.text todo.Description
                                ]
                    ]
                ]

                todoAction model dispatch
            ]
        ]

    let collisionRunSummary (pageModel: CollisionRunPage.Model) =
        match pageModel.Details with
        | NotStarted ->
            Html.p [ prop.className "text-slate-800"; prop.text "Run details have not been loaded yet." ]
        | Loading _ ->
            Html.p [ prop.className "text-slate-800"; prop.text $"Loading collision run {pageModel.RunId}..." ]
        | Loaded details ->
            let summary = CollisionRunPage.createSummaryViewModel details

            Html.div [
                prop.className "flex flex-col gap-4"
                prop.children [
                    Html.div [
                        prop.className "grid gap-3 sm:grid-cols-2"
                        prop.children [
                            summaryValue "Run Id" summary.RunId
                            summaryValue "Patient Id" summary.PatientId
                            summaryValue "Plan Id" summary.PlanId
                            summaryValue "Status" summary.StatusText
                            summaryValue "Generated Points" (summary.GeneratedPointCount |> Option.defaultValue 0 |> string)
                            summaryValue "Candidate Points" (summary.CandidatePointCount |> Option.defaultValue 0 |> string)
                            summaryValue "Inside Points" (summary.InsidePointCount |> Option.defaultValue 0 |> string)
                        ]
                    ]
                    Html.div [
                        prop.className "rounded-lg bg-white/20 px-4 py-3 border border-white/30"
                        prop.children [
                            Html.p [ prop.className "text-xs uppercase tracking-wide text-slate-700"; prop.text "Beams" ]
                            Html.ul [
                                prop.className "mt-2 list-disc ml-5 text-slate-900"
                                prop.children [
                                    for beamId in summary.BeamIds do
                                        Html.li [ prop.text beamId ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

let view model dispatch =
    Html.section [
        prop.className "h-screen w-screen relative overflow-hidden"
        prop.children [
            // Meta viewport tag for proper mobile scaling
            Html.meta [
                prop.name "viewport"
                prop.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
            ]
            
            // Background div with image and glass effect
            Html.div [
                prop.className "absolute inset-0 bg-cover bg-center bg-fixed bg-no-repeat
                bg-white/20 backdrop-blur-sm"
                prop.style [
                    style.backgroundImageUrl "https://unsplash.it/1200/900?random"
                ]
            ]

            // Content container (the rest of your UI)
            Html.div [
                prop.className "relative z-10 h-full w-full"
                prop.children [
                    // Your existing content here
                    Html.a [
                        prop.href "https://safe-stack.github.io/"
                        prop.className "absolute block ml-4 sm:ml-12 h-10 w-10 sm:h-12 sm:w-12 bg-teal-300 hover:cursor-pointer hover:bg-teal-400"
                        prop.children [
                            Html.img [ prop.src "/favicon.png"; prop.alt "Logo" ]
                        ]
                    ]


                    Html.div [
                        prop.className "flex flex-col items-center justify-center h-full"
                        prop.children [
                            Html.div [
                                prop.className "bg-white/20 backdrop-blur-lg p-4 sm:p-8 rounded-xl shadow-lg border border-white/30 mx-4 sm:mx-0 max-w-full sm:max-w-2xl"
                                prop.children [
                                    Html.h1 [
                                        prop.className "text-center text-3xl sm:text-5xl font-bold mb-3 p-2 sm:p-4"
                                        prop.text "CollisionAvoidance"
                                    ]
                                    match model.Page with
                                    | TodoPage -> ViewComponents.todoList model dispatch
                                    | CollisionPage pageModel ->
                                        ViewComponents.collisionRunSummary pageModel
                                        match pageModel.ErrorMessage with
                                        | Some errorMessage ->
                                            Html.p [ prop.className "mt-4 text-sm text-red-700"; prop.text errorMessage ]
                                        | None -> Html.none
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
