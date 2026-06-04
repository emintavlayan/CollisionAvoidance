module BeamSampling

open Shared

type BeamAxisSample = {
    BeamId: string
    BeamName: string option
    ControlPointIndex: int option
    GantryAngle: float option
    PatientSupportAngle: float
    Isocenter: Point3D
    SourcePosition: Point3D
}

/// Returns the configured gantry step in degrees, or a conservative default.
let getArcStepDegrees (settings: SamplingSettingsDto) =
    settings.ArcStepDegrees |> Option.defaultValue 1.0

/// Generates a normalized list of gantry angles from beam metadata.
let generateGantrySteps (settings: SamplingSettingsDto) (beam: BeamSnapshotDto) =
    let step = abs (getArcStepDegrees settings)

    match beam.GantryStart, beam.GantryStop with
    | Some startAngle, Some stopAngle when step > 0.0 ->
        match beam.GantryDirection with
        | CounterClockwise when startAngle < stopAngle ->
            [ startAngle .. step .. stopAngle ]
        | CounterClockwise ->
            [ startAngle .. step .. 360.0 ] @ [ 0.0 .. step .. stopAngle ]
        | Clockwise when startAngle > stopAngle ->
            [ startAngle .. -step .. stopAngle ]
        | Clockwise ->
            [ startAngle .. -step .. 0.0 ] @ [ 360.0 .. -step .. stopAngle ]
        | NotSpecified ->
            [ startAngle; stopAngle ]
    | Some startAngle, Some stopAngle ->
        [ startAngle; stopAngle ]
    | Some angle, None
    | None, Some angle -> [ angle ]
    | None, None -> beam.ControlPoints |> List.map (fun controlPoint -> controlPoint.GantryAngle)

/// Creates one beam-axis sample from beam-level positions when control-point positions are unavailable.
let createBeamLevelSample (beam: BeamSnapshotDto) : Result<BeamAxisSample, string> =
    match beam.Isocenter, beam.SourcePosition with
    | Some isocenter, Some sourcePosition ->
        Ok {
            BeamId = beam.BeamId
            BeamName = beam.BeamName
            ControlPointIndex = None
            GantryAngle = beam.GantryStart
            PatientSupportAngle = beam.PatientSupportAngle |> Option.defaultValue 0.0
            Isocenter = isocenter
            SourcePosition = sourcePosition
        }
    | _ ->
        Error $"Beam {beam.BeamId} does not have enough detached source-position data for sampling."

/// Creates one beam-axis sample from a detached control point.
let createControlPointSample (beam: BeamSnapshotDto) (controlPoint: ControlPointSnapshotDto) : Result<BeamAxisSample, string> =
    match controlPoint.Isocenter, controlPoint.SourcePosition with
    | Some isocenter, Some sourcePosition ->
        Ok {
            BeamId = beam.BeamId
            BeamName = beam.BeamName
            ControlPointIndex = Some controlPoint.Index
            GantryAngle = Some controlPoint.GantryAngle
            PatientSupportAngle =
                controlPoint.PatientSupportAngle
                |> Option.orElse beam.PatientSupportAngle
                |> Option.defaultValue 0.0
            Isocenter = isocenter
            SourcePosition = sourcePosition
        }
    | _ ->
        Error $"Beam {beam.BeamId} control point {controlPoint.Index} is missing source-position or isocenter data."

/// Creates all available beam-axis samples for a detached beam snapshot.
let createBeamAxisSamples (_settings: SamplingSettingsDto) (beam: BeamSnapshotDto) : Result<BeamAxisSample list, string> =
    if not beam.ControlPoints.IsEmpty then
        beam.ControlPoints
        |> List.map (createControlPointSample beam)
        |> List.fold
            (fun state next ->
                match state, next with
                | Ok samples, Ok sample -> Ok (sample :: samples)
                | Error error, _ -> Error error
                | _, Error error -> Error error)
            (Ok [])
        |> Result.map List.rev
    else
        createBeamLevelSample beam |> Result.map List.singleton
