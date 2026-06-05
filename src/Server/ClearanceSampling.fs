module ClearanceSampling

open System
open FsToolkit.ErrorHandling
open Shared
open GeometryMath
open BeamSampling

/// Represents one generated clearance-sampling point before BODY volume filtering.
type CollisionPointCandidate = {
    Location: Point3D
    Source: CollisionPointSourceDto
}

/// Returns the beam-axis offset used when placing clearance samples.
let getBeamAxisOffsetMm (settings: SamplingSettingsDto) =
    settings.BeamAxisOffsetMm
    |> Option.orElse settings.ClearanceDistanceMm
    |> Option.defaultValue (Length.millimeters 550.0)

/// Returns the clearance radius used for line and cap generation.
let getClearanceRadiusMm (settings: SamplingSettingsDto) =
    settings.ClearanceRadiusMm
    |> Option.orElse settings.ClearanceDistanceMm
    |> Option.defaultValue (Length.millimeters 390.0)

/// Returns the preferred sampling resolution for clearance generation.
let getSampleResolutionMm (settings: SamplingSettingsDto) =
    settings.BeamSampleStepMm |> Option.defaultValue (Length.millimeters 5.0)

/// Builds a candidate-point source payload from one beam-axis sample.
let createPointSource (sampleType: CollisionPointSampleTypeDto) (sample: BeamAxisSample) = {
    BeamId = sample.BeamId
    ControlPointIndex = sample.ControlPointIndex
    GantryAngle = sample.GantryAngle
    SampleType = sampleType
    AccessoryId = None
}

/// Creates two orthonormal basis vectors that span the plane perpendicular to the beam axis.
let createPerpendicularBasis (direction: Vector3D) : Result<Vector3D * Vector3D, string> =
    let up =
        if abs direction.Z < 0.99 then
            createVector 0.0 0.0 1.0
        else
            createVector 0.0 1.0 0.0

    result {
        let! firstBasis = crossProduct direction up |> normalizeVector
        let! secondBasis = crossProduct direction firstBasis |> normalizeVector
        return firstBasis, secondBasis
    }

/// Generates line-sample candidates from one beam-axis sample.
let generateLineSampleCandidates (settings: SamplingSettingsDto) (sample: BeamAxisSample) : Result<CollisionPointCandidate list, string> =
    result {
        let radius = getClearanceRadiusMm settings |> Length.mmToCm |> Length.toFloatCm
        let offset = getBeamAxisOffsetMm settings |> Length.mmToCm |> Length.toFloatCm
        let resolution = getSampleResolutionMm settings |> Length.mmToCm |> Length.toFloatCm
        let direction = vectorBetween sample.Isocenter sample.SourcePosition
        let! normalizedDirection = normalizeVector direction
        let lineCenter = translatePoint sample.Isocenter (scaleVector offset normalizedDirection)
        let angleRadians = Math.PI / 180.0 * sample.PatientSupportAngle
        let pointCount = max 1 (int (radius * 2.0 / resolution))

        return
            [ 0 .. pointCount ]
            |> List.map (fun index ->
                let lineDistance = radius * 2.0 * float index / float pointCount - radius
                let offsetVector =
                    createVector (Math.Sin(angleRadians) * lineDistance) 0.0 (Math.Cos(angleRadians) * lineDistance)

                {
                    Location = translatePoint lineCenter offsetVector
                    Source = createPointSource LineSample sample
                })
    }

/// Generates half-disk cap candidates from one beam-axis sample.
let generateHalfDiskCapCandidates
    (settings: SamplingSettingsDto)
    (sampleType: CollisionPointSampleTypeDto)
    (isFirstCap: bool)
    (sample: BeamAxisSample)
    : Result<CollisionPointCandidate list, string> =
    result {
        let radius = getClearanceRadiusMm settings |> Length.mmToCm |> Length.toFloatCm
        let resolution = getSampleResolutionMm settings |> Length.mmToCm |> Length.toFloatCm
        let offset = getBeamAxisOffsetMm settings |> Length.mmToCm |> Length.toFloatCm
        let direction = vectorBetween sample.Isocenter sample.SourcePosition
        let! normalizedDirection = normalizeVector direction
        let! firstBasis, secondBasis = createPerpendicularBasis normalizedDirection
        let diskCenter = translatePoint sample.Isocenter (scaleVector offset normalizedDirection)
        let angleRadians = Math.PI / 180.0 * sample.PatientSupportAngle
        let halfValue =
            if isFirstCap then
                Math.PI * 1.5 + angleRadians
            else
                Math.PI * 0.5 - angleRadians

        let radii =
            [ 0.0 .. resolution .. radius ]
            |> fun values ->
                if values |> List.tryLast = Some radius then
                    values
                else
                    values @ [ radius ]

        return
            radii
            |> List.collect (fun currentRadius ->
                let pointsPerDisk = max 1 (int (Math.PI * currentRadius / resolution + 1.0))

                [ 0 .. pointsPerDisk ]
                |> List.map (fun index ->
                    let angle = Math.PI * float index / float pointsPerDisk + halfValue
                    let radialOffset =
                        addVectors
                            (scaleVector (currentRadius * Math.Cos angle) firstBasis)
                            (scaleVector (currentRadius * Math.Sin angle) secondBasis)

                    {
                        Location = translatePoint diskCenter radialOffset
                        Source = createPointSource sampleType sample
                    }))
    }

/// Generates all clearance sample candidates for one detached beam snapshot.
let generateClearanceSamplePointsForBeam (settings: SamplingSettingsDto) (beam: BeamSnapshotDto) : Result<CollisionPointCandidate list, string> =
    result {
        let! beamSamples = createBeamAxisSamples settings beam

        let! linePoints =
            beamSamples
            |> List.map (generateLineSampleCandidates settings)
            |> List.fold
                (fun state next ->
                    match state, next with
                    | Ok points, Ok nextPoints -> Ok (nextPoints @ points)
                    | Error error, _ -> Error error
                    | _, Error error -> Error error)
                (Ok [])

        let! capPoints =
            match beamSamples with
            | [] -> Ok []
            | [ singleSample ] ->
                generateHalfDiskCapCandidates settings FirstCapSample true singleSample
            | firstSample :: remainingSamples ->
                result {
                    let lastSample = List.last remainingSamples
                    let! firstCap = generateHalfDiskCapCandidates settings FirstCapSample true firstSample
                    let! lastCap = generateHalfDiskCapCandidates settings LastCapSample false lastSample
                    return firstCap @ lastCap
                }

        return linePoints @ capPoints
    }

/// Generates all clearance sample candidates for the detached collision request.
let generateClearanceSamplePoints (request: CollisionRunRequestDto) : Result<CollisionPointCandidate list, string> =
    request.Plan.Beams
    |> List.map (generateClearanceSamplePointsForBeam request.SamplingSettings)
    |> List.fold
        (fun state next ->
            match state, next with
            | Ok points, Ok nextPoints -> Ok (nextPoints @ points)
            | Error error, _ -> Error error
            | _, Error error -> Error error)
        (Ok [])
