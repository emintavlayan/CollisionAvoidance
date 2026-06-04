module CollisionAnalysis

open System
open Shared

let createPendingSummary (runId: Guid) (request: CollisionRunRequestDto) = {
    RunId = runId
    CreatedAtUtc = DateTime.UtcNow
    Status = AnalysisPending
    FlatResult = None
    DetailedResult = None
    BeamResults =
        request.Plan.Beams
        |> List.map (fun beam -> {
            BeamId = beam.BeamId
            BeamName = beam.BeamName
            Status = AnalysisPending
            ControlPointResults =
                beam.ControlPoints
                |> List.map (fun controlPoint -> {
                    ControlPointIndex = controlPoint.Index
                    GantryAngle = Some controlPoint.GantryAngle
                    Status = AnalysisPending
                    CollisionPoints = []
                })
        })
}
