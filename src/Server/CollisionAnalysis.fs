module CollisionAnalysis

open System
open Shared

let createPendingSummary (runId: Guid) (request: CollisionRunRequestDto) = {
    RunId = runId
    CreatedAtUtc = DateTime.UtcNow
    Status = AnalysisPending
    BeamResults =
        request.Plan.Beams
        |> List.map (fun beam -> {
            BeamId = beam.BeamId
            Status = AnalysisPending
            ControlPointResults =
                beam.ControlPoints
                |> List.map (fun controlPoint -> {
                    ControlPointIndex = controlPoint.Index
                    Status = AnalysisPending
                    CollisionPoints = []
                })
        })
}
