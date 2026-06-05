module CollisionRunPage

open System
open SAFE
open Shared

/// Represents the client-side state for one collision-run summary page.
type Model = {
    RunId: Guid
    Details: RemoteData<CollisionRunDetailsDto>
    ErrorMessage: string option
}

/// Represents the flattened collision-run summary values shown by the current client page.
type SummaryViewModel = {
    RunId: string
    PatientId: string
    PlanId: string
    StatusText: string
    GeneratedPointCount: int option
    CandidatePointCount: int option
    InsidePointCount: int option
    BeamIds: string list
}

/// Parses a collision run id from a pathname such as `/collision/{runId}`.
let tryParseRunIdFromPath (path: string) =
    let segments =
        path.Trim('/')
            .Split('/', StringSplitOptions.RemoveEmptyEntries)

    match segments with
    | [| "collision"; runIdText |] ->
        match Guid.TryParse runIdText with
        | true, runId -> Some runId
        | false, _ -> None
    | _ -> None

/// Converts a collision status into a short display string.
let statusToText status =
    match status with
    | AnalysisPending -> "Pending"
    | NoCollision -> "No collision"
    | CollisionDetected -> "Collision detected"
    | AnalysisError error -> $"Analysis error: {error}"

/// Creates the initial collision-run page model for one run id.
let init runId = {
    RunId = runId
    Details = NotStarted
    ErrorMessage = None
}

/// Creates a minimal summary view model from stored collision run details.
let createSummaryViewModel (details: CollisionRunDetailsDto) = {
    RunId = details.RunId.ToString()
    PatientId = details.Request.Plan.PatientId
    PlanId = details.Request.Plan.PlanId
    StatusText = statusToText details.Summary.Status
    GeneratedPointCount = details.Summary.FlatResult |> Option.map (fun result -> result.GeneratedPointCount)
    CandidatePointCount = details.Summary.FlatResult |> Option.map (fun result -> result.BoundingBoxCandidateCount)
    InsidePointCount = details.Summary.FlatResult |> Option.map (fun result -> result.InsidePointCount)
    BeamIds = details.Request.Plan.Beams |> List.map (fun beam -> beam.BeamId)
}
