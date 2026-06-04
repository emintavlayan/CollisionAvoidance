module ResultSummary

open Shared

let beamStatuses (summary: CollisionRunSummaryDto) =
    summary.BeamResults |> List.map (fun result -> result.BeamId, result.Status)
