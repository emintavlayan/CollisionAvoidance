module CollisionApi

open System
open System.IO
open System.Text.Json
open Giraffe
open Microsoft.AspNetCore.Http
open System.Text.Json.Serialization
open Shared

let private serializerOptions =
    let options = JsonSerializerOptions(JsonSerializerDefaults.Web)
    options.Converters.Add(JsonFSharpConverter())
    options

/// Reads a detached JSON payload from the request body using F#-aware JSON settings.
let readJson<'T> (ctx: HttpContext) =
    task {
        use reader = new StreamReader(ctx.Request.Body)
        let! body = reader.ReadToEndAsync()
        return JsonSerializer.Deserialize<'T>(body, serializerOptions)
    }

/// Writes one detached payload to the HTTP response as JSON.
let writeJson (statusCode: int) (payload: 'T) : HttpHandler =
    fun _ ctx ->
        task {
            ctx.Response.StatusCode <- statusCode
            ctx.Response.ContentType <- "application/json; charset=utf-8"

            let responseBody = JsonSerializer.Serialize(payload, serializerOptions)
            do! ctx.Response.WriteAsync(responseBody)
            return Some ctx
        }

/// Writes a detached error message to the HTTP response as JSON.
let writeError (statusCode: int) (message: string) : HttpHandler =
    writeJson statusCode {| error = message |}

/// Creates a collision run from detached request DTO data and returns its stored summary.
let createCollisionRunHandler : HttpHandler =
    fun next ctx ->
        task {
            try
                let! request = readJson<CollisionRunRequestDto> ctx
                match RunStore.createRun request with
                | Ok response ->
                    return! writeJson StatusCodes.Status201Created response next ctx
                | Error error ->
                    return! writeError StatusCodes.Status400BadRequest error next ctx
            with error ->
                return! writeError StatusCodes.Status400BadRequest error.Message next ctx
        }

/// Returns the stored flat-summary payload for one collision run.
let getCollisionRunSummaryHandler (runId: Guid) : HttpHandler =
    fun next ctx ->
        task {
            match RunStore.tryGetRun runId with
            | Some record ->
                return! writeJson StatusCodes.Status200OK record.Summary next ctx
            | None ->
                return! writeError StatusCodes.Status404NotFound $"Collision run {runId} was not found." next ctx
        }

/// Returns the stored request and summary payload for one collision run.
let getCollisionRunDetailsHandler (runId: Guid) : HttpHandler =
    fun next ctx ->
        task {
            match RunStore.tryGetRunDetails runId with
            | Some details ->
                return! writeJson StatusCodes.Status200OK details next ctx
            | None ->
                return! writeError StatusCodes.Status404NotFound $"Collision run {runId} was not found." next ctx
        }
