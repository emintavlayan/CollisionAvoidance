(**
    Module: VMS.TPS.BeamGeometry
    Purpose: 
        - Provides helper functions for extracting and manipulating geometric information 
          from beams in the Varian Eclipse Scripting API (ESAPI).
        - Includes utilities for gantry angle retrieval, arc start/stop normalization, 
          arc classification, and generation of discrete gantry steps.
        - Supports extraction of isocenter/source position pairs as arrays, enabling 
          efficient parallel execution.

    Why:
        - Centralizes beam geometry logic to avoid duplication across scripts.
        - Ensures consistent interpretation of gantry angles regardless of beam direction.
        - Facilitates step-based processing (e.g., sampling source positions along arcs).

    Notes:
        - All angles are in degrees.
        - Arc start/stop extraction always normalizes to clockwise motion for consistency.
        - Depends on VMS.TPS.Common.Model.API for Beam and related core types.
        - Depends on VMS.TPS.Common.Model.API for Beam and related core types.
*)

module VMS.TPS.BeamGeometry

open VMS.TPS.Common.Model.Types
open VMS.TPS.Common.Model.API

/// Extracts the gantry start angle from the first control point of the beam.
let getGantryStartAngle (beam : Beam) =
    beam.ControlPoints
    |> Seq.head
    |> fun cp -> cp.GantryAngle

/// Extracts the gantry stop angle from the last control point of the beam.
let getGantryStopAngle (beam : Beam) =
    beam.ControlPoints
    |> Seq.last
    |> fun cp -> cp.GantryAngle

/// Returns the (start, stop) gantry angles for an arc, as if it moves clockwise.
/// If the beam is defined as counterclockwise, the start/stop are swapped
/// so the result is always in the order a clockwise traversal would produce.
let getGantryStartStopFromArcAsIfClockWise (arc : Beam) =
    let gantryStart =
        arc |> getGantryStartAngle

    let gantryStop =
        arc |> getGantryStopAngle

    match arc.GantryDirection with
    | GantryDirection.CounterClockwise -> (gantryStop, gantryStart)
    | _ -> (gantryStart, gantryStop)

/// Active pattern for classifying arcs by the region of the gantry rotation:
/// - LeftArc: Entirely in the [180°, 360°) half
/// - RightArc: Entirely in the [0°, 180°) half
/// - CrossingArc: Spans across 0° (wrap-around) between right and left halves
let (|LeftArc|RightArc|CrossingArc|) (arc : Beam) =
    let start, stop =
        arc
        |> getGantryStartStopFromArcAsIfClockWise

    if start >= 180.0 then
        if (stop <= 180 || stop > 0.0) then CrossingArc else LeftArc
    else
        RightArc

/// Generates gantry angles from start to stop, stepping CW by the given step size.
/// Always includes the start and stop angles.
/// Steps are aligned to a fixed 0°-based grid, making it possible to merge results
/// from multiple arcs and deduplicate them.
/// Handles wrap-around at 360° cleanly.
let generateGantryAngleStepsFromArc (step : float) (arc : Beam) =
    // Get arc start/stop angles assuming clockwise motion
    let start, stop =
        arc
        |> getGantryStartStopFromArcAsIfClockWise

    let step =
        abs step // ensure positive step size

    let rem =
        start % step // offset from step grid (e.g. 273° on 5° grid → 3° offset)

    // First aligned step after start
    let firstStep =
        if rem = 0.0 then
            start + step // already aligned → next step
        else
            start + (step - rem) // align up to next grid point

    match arc with
    | RightArc
    | LeftArc ->
        start
        :: [ firstStep..step..stop ] // step through arc normally

    | CrossingArc ->
        let firstPart = [ start..step .. 359.9 ] // wrap to 360
        let secondPart = [ 0.0 .. step .. stop ] // continue from 0 to stop
        firstPart @ secondPart

/// Returns an array of `(isocenter, sourcePosition)` tuples for the given beam.
/// For static beams (`GantryDirection.None`), the array contains a single tuple.
/// For arc beams, generates gantry angles using a fixed step size, computes
/// source positions for each angle, and pairs each with the same static isocenter.
/// The array output is convenient for later parallelized processing.
let extractSourcePositions (step : float) (beam : Beam) =

    let isocenterPosition =
        beam.IsocenterPosition

    match beam.GantryDirection with
    | GantryDirection.None -> // Static beam, single source position
        let gantry =
            beam |> getGantryStartAngle

        let sourcePosition =
            beam.GetSourceLocation gantry

        [| (isocenterPosition, sourcePosition) |] // single-element array

    | GantryDirection.Clockwise
    | GantryDirection.CounterClockwise -> // Arc beam, generate gantry angles
        let gantryAngles =
            beam
            |> generateGantryAngleStepsFromArc step

        gantryAngles
        |> List.map (fun g -> beam.GetSourceLocation g) // list of source positions
        |> List.map (fun src -> (isocenterPosition, src)) // list of tuples (isocenter, source position)
        |> List.toArray // array is better for upcoming paralel execution

    //Fallback for unexpected/future enum values — required for .NET exhaustiveness, should never occur.
    | _ -> failwithf "Unsupported GantryDirection: %A" beam.GantryDirection

/// Safely extracts source positions, converting unexpected errors into Result.
let tryExtractSourcePositions
    (step : float)
    (beam : Beam)
    : Result<(VVector * VVector)[], string>
    =
    try
        extractSourcePositions step beam
        |> Ok
    with ex ->
        Error($"Failed to extract source positions: {ex.Message}")
