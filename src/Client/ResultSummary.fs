module ResultSummary

open Shared

/// Returns the flat collision counts when they are available on the summary.
let flatCounts (summary: CollisionRunSummaryDto) =
    summary.FlatResult
    |> Option.map (fun result -> result.GeneratedPointCount, result.BoundingBoxCandidateCount, result.InsidePointCount)

/// Returns the detached beam ids and statuses present on the summary.
let beamStatuses (summary: CollisionRunSummaryDto) =
    summary.BeamResults |> List.map (fun result -> result.BeamId, result.Status)
