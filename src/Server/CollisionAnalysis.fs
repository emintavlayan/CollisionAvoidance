module CollisionAnalysis

open System
open Shared
open ClearanceSampling
open BodyVolume

/// Creates a pending collision summary that preserves beam and control-point structure.
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

/// Creates a detached collision point DTO from one sampled candidate.
let createCollisionPoint (candidate: CollisionPointCandidate) = {
    Location = candidate.Location
    DistanceMm = None
    Description = Some "Clearance sample lies inside the BODY contour volume."
    Source = Some candidate.Source
}

/// Determines the collision status from the number of inside points.
let determineStatus insidePointCount =
    if insidePointCount > 0 then
        CollisionDetected
    else
        NoCollision

/// Groups collision points into per-control-point result records for one beam.
let createControlPointResults (beam: BeamSnapshotDto) (collisionPoints: CollisionPointDto list) =
    let groupedPoints =
        collisionPoints
        |> List.groupBy (fun collisionPoint -> collisionPoint.Source |> Option.bind (fun source -> source.ControlPointIndex))
        |> Map.ofList

    match beam.ControlPoints with
    | [] ->
        [
            {
                ControlPointIndex = -1
                GantryAngle = beam.GantryStart
                Status = if collisionPoints.IsEmpty then NoCollision else CollisionDetected
                CollisionPoints = collisionPoints
            }
        ]
    | controlPoints ->
        controlPoints
        |> List.map (fun controlPoint ->
            let points = groupedPoints |> Map.tryFind (Some controlPoint.Index) |> Option.defaultValue []

            {
                ControlPointIndex = controlPoint.Index
                GantryAngle = Some controlPoint.GantryAngle
                Status = if points.IsEmpty then NoCollision else CollisionDetected
                CollisionPoints = points
            })

/// Groups collision points into per-beam result records.
let createBeamResults (request: CollisionRunRequestDto) (collisionPoints: CollisionPointDto list) =
    request.Plan.Beams
    |> List.map (fun beam ->
        let beamPoints =
            collisionPoints
            |> List.filter (fun collisionPoint ->
                collisionPoint.Source
                |> Option.exists (fun source -> source.BeamId = beam.BeamId))

        {
            BeamId = beam.BeamId
            BeamName = beam.BeamName
            Status = if beamPoints.IsEmpty then NoCollision else CollisionDetected
            ControlPointResults = createControlPointResults beam beamPoints
        })

/// Creates the fast flat collision result for a detached collision run request.
let createFlatResult (request: CollisionRunRequestDto) : Result<FlatCollisionResultDto, string> =
    let stopwatch = Diagnostics.Stopwatch.StartNew()

    request
    |> generateClearanceSamplePoints
    |> Result.map (fun generatedCandidates ->
        let boundedCandidates = filterCandidatesByBounds request.Body generatedCandidates
        let insideCandidates = filterCandidatesInsideBody request.Body boundedCandidates
        stopwatch.Stop()

        {
            Status = determineStatus insideCandidates.Length
            GeneratedPointCount = generatedCandidates.Length
            BoundingBoxCandidateCount = boundedCandidates.Length
            InsidePointCount = insideCandidates.Length
            ElapsedMs = Some stopwatch.Elapsed.TotalMilliseconds
        })

/// Creates the detailed collision result with per-point provenance.
let createDetailedResult (request: CollisionRunRequestDto) : Result<DetailedCollisionResultDto, string> =
    request
    |> generateClearanceSamplePoints
    |> Result.map (fun generatedCandidates ->
        let boundedCandidates = filterCandidatesByBounds request.Body generatedCandidates
        let insideCandidates = filterCandidatesInsideBody request.Body boundedCandidates
        let collisionPoints = insideCandidates |> List.map createCollisionPoint
        let beamResults = createBeamResults request collisionPoints
        let controlPointResults = beamResults |> List.collect (fun beamResult -> beamResult.ControlPointResults)

        {
            Status = determineStatus collisionPoints.Length
            BeamResults = beamResults
            ControlPointResults = controlPointResults
            CollisionPoints = collisionPoints
        })
