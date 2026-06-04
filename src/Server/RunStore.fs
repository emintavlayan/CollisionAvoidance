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

/// Creates a collision run, executes the default flat analysis, stores the result, and returns the create response.
let createRun (request: CollisionRunRequestDto) =
    let runId = Guid.NewGuid()
    let pendingSummary = CollisionAnalysis.createPendingSummary runId request

    let completedSummary =
        match CollisionAnalysis.createFlatResult request with
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
        Request = request
        Summary = completedSummary
    }

    runs[runId] <- record

    {
        RunId = runId
        RunUrl = Some $"/collision/{runId}"
        Summary = completedSummary
    }

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
let tryGetRunDetails (runId: Guid) =
    tryGetRun runId
    |> Option.map (fun record -> {
        RunId = record.RunId
        Request = record.Request
        Summary = record.Summary
    })
