module RunStore

open System
open System.Collections.Concurrent
open Shared

type CollisionRunRecord = {
    RunId: Guid
    Request: CollisionRunRequestDto
    Summary: CollisionRunSummaryDto
}

let private runs = ConcurrentDictionary<Guid, CollisionRunRecord>()

/// Checks whether a detached BODY snapshot is present and minimally usable for first-version analysis.
let validateBodySnapshot (body: BodySnapshotDto) =
    if String.IsNullOrWhiteSpace body.StructureId then
        Error "Collision run request is missing BODY."
    elif not (String.Equals(body.StructureId, "BODY", StringComparison.OrdinalIgnoreCase)) then
        Error "Collision run request BODY structure must be identified as BODY."
    elif body.ContourSlices.IsEmpty && body.Mesh.IsNone then
        Error "Collision run request BODY structure does not contain contour or mesh data."
    else
        Ok body

/// Checks whether a detached collision request contains the minimum first-version analysis inputs.
let validateCollisionRunRequest (request: CollisionRunRequestDto) =
    match validateBodySnapshot request.Body with
    | Error error -> Error error
    | Ok _body ->
        if request.Plan.Beams.IsEmpty then
            Error "Collision run request does not contain any beams."
        else
            Ok request

/// Creates a collision run, executes the default flat analysis, stores the result, and returns the create response.
let createRun (request: CollisionRunRequestDto) =
    validateCollisionRunRequest request
    |> Result.map (fun validRequest ->
        let runId = Guid.NewGuid()
        let pendingSummary = CollisionAnalysis.createPendingSummary runId validRequest

        let completedSummary =
            match CollisionAnalysis.createFlatResult validRequest with
            | Ok flatResult ->
                {
                    pendingSummary with
                        Status = flatResult.Status
                        FlatResult = Some flatResult
                }
            | Error error ->
                {
                    pendingSummary with
                        Status = AnalysisError error
                }

        let record = {
            RunId = runId
            Request = validRequest
            Summary = completedSummary
        }

        runs[runId] <- record

        {
            RunId = runId
            RunUrl = Some $"/collision/{runId}"
            Summary = completedSummary
        })

/// Tries to retrieve one stored collision run by its identifier.
let tryGetRun (runId: Guid) =
    runs.TryGetValue runId
    |> function
        | true, record -> Some record
        | false, _ -> None

/// Updates the stored summary for one collision run.
let updateSummary (runId: Guid) (summary: CollisionRunSummaryDto) =
    match tryGetRun runId with
    | Some record ->
        let updatedRecord = { record with Summary = summary }
        runs[runId] <- updatedRecord
        Ok updatedRecord
    | None ->
        Error $"Collision run {runId} was not found."

/// Lists all stored collision runs in memory.
let listRuns () =
    runs.Values |> Seq.toList

/// Tries to retrieve the detached request and summary payload for one collision run.
let tryGetRunDetails (runId: Guid) : CollisionRunDetailsDto option =
    tryGetRun runId
    |> Option.map (fun record -> {
        RunId = record.RunId
        Request = record.Request
        Summary = record.Summary
    })
