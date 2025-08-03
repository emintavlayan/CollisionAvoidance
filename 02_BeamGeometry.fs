module VMS.TPS.BeamGeometry

open VMS.TPS.Common.Model.Types
open VMS.TPS.Common.Model.API

type Direction = CW | CCW | NoneDir

type Vector3 = { X: float; Y: float; Z: float }

type BeamGeometry = {
    BeamId: string
    Origin: VVector
    CouchAngle: float
    Start: VVector
    Stop: VVector
}


// Convert to a canonical order: for CCW, flip start/stop
let normalizeGeometry (beam: Beam) =
    match beam.Direction with
    | CCW -> (beam.Stop, beam.Start)
    | _   -> (beam.Start, beam.Stop)

let pickBeamIdsWithUniqueGeometry (beams: Beam[]) : BeamGeometry[] =
    beams
    |> Array.map (fun beam ->
        let (start, stop) = normalizeGeometry beam
        {
            BeamId = beam.Id
            Origin = beam.Origin
            CouchAngle = beam.CouchAngle
            CanonicalStart = start
            CanonicalStop = stop
        })
    |> Array.distinctBy (fun bg ->
        (bg.Origin, bg.CouchAngle, bg.CanonicalStart, bg.CanonicalStop))
   
// Simulate API lookup if needed
let getBeamById (id: string) (beams: Beam[]) =
    beams |> Array.find (fun b -> b.Id = id)

let extractSourcePositions (beams: Beam[]) (beamGeometries: BeamGeometry[]) : (Vector3 * Vector3)[] =
    beamGeometries
    |> Array.collect (fun bg ->
        let beam = getBeamById bg.BeamId beams
        match beam.Direction with
        | NoneDir ->
            [| (beam.Start, beam.Stop) |] // static beam, single tuple
        | _ ->
            // Here you'd replace this with actual arc step logic from your API
            // Placeholder: simulate multiple (a, b) pairs
            [| (beam.Start, beam.Stop) |] // or multiple if needed
    )

let runGeometryExtraction (allBeams: Beam[]) =
    allBeams
    |> pickBeamIdsWithUniqueGeometry               // Deduplicate based on canonical geometry
    |> extractSourcePositions allBeams            // Expand into list of (a, b) tuples
    |> Array.iter (fun (a, b) ->
        printfn "From (%f, %f, %f) to (%f, %f, %f)" 
            a.X a.Y a.Z b.X b.Y b.Z)



// ---

let generateGantrySteps start stop step =
    let step = abs step
    let rem = start % step
    let first =
        if rem = 0.0 then start + step
        else start + (step - rem)

    start :: [first .. step .. stop]

// ===============================================
// Gantry Arc Stepping and Segment Extraction
// ===============================================

type Direction = CW | CCW | NoneDir

type Vector3 = { X: float; Y: float; Z: float }

type Beam = {
    Id: string
    StartAngle: float
    StopAngle: float
    Direction: Direction
    Origin: string
    CouchAngle: float
    // ...other properties as needed
}

// Arc type classification for clear wrap handling
type ArcType =
    | LeftArc      // both angles > 180°
    | RightArc     // both angles ≤ 180°
    | CrossArc     // wraps around 360°, crosses from >180 to <180

/// Classifies a beam's arc shape based on start and stop angle positions.
let classifyArc (start: float) (stop: float) : ArcType =
    match start with
    | s when s > 180.0 ->
        match stop with
        | t when t < 360.0 -> LeftArc
        | _ -> CrossArc
    | _ -> RightArc

/// Generates all gantry angles from start to stop, stepping CW by given step size.
/// Always includes start and stop. Internals align to the step grid.
let generateWrappedGantryAngles (start: float) (stop: float) (step: float) : float[] =
    let step = abs step
    let rem = start % step

    // First angle after start that aligns with the step grid
    let firstStep =
        if rem = 0.0 then start + step
        else start + (step - rem)

    match classifyArc start stop with
    | RightArc
    | LeftArc ->
        start :: [firstStep .. step .. stop]

    | CrossArc ->
        let firstPart = [start .. step .. 355.0]  // up to just before 360
        let secondStart = 0.0
        let secondRem = secondStart % step
        let secondFirst =
            if secondRem = 0.0 then secondStart
            else secondStart + (step - secondRem)
        let secondPart = [secondFirst .. step .. stop]

        (firstPart @ secondPart)
        |> List.distinct
        |> fun steps -> start :: (steps |> List.filter (fun a -> a <> start && a <> stop)) @ [stop]

    |> Array.ofList

/// Normalizes all arc beams to CW direction by flipping angles if needed.
let normalizeArcDirection (beam: Beam) : float * float =
    match beam.Direction with
    | CCW -> (beam.StopAngle, beam.StartAngle) // flip to make CW
    | _   -> (beam.StartAngle, beam.StopAngle)

/// Generates angle pairs (g1, g2) from a list of gantry angles.
let generateAnglePairs (angles: float[]) : (float * float)[] =
    angles
    |> Array.windowed 2
    |> Array.map (fun [|a; b|] -> (a, b))

/// Placeholder: convert (beam, angle) → 3D point in space.
/// TO IMPLEMENT: Use your API to map gantry angle to a 3D position.
let angleToPoint (beam: Beam) (gantryAngle: float) : Vector3 =
    // IMPLEMENT ME: use actual geometry logic or API call
    failwith "angleToPoint not implemented"

/// Generates all (a, b) segments from a beam, normalized to CW arcs.
/// Handles static (NoneDir) beams as single segment.
let generateArcSegments (beam: Beam) (step: float) : (Vector3 * Vector3)[] =
    if beam.Direction = NoneDir then
        let a = angleToPoint (beam, beam.StartAngle)
        let b = angleToPoint (beam, beam.StopAngle)
        [| (a, b) |]
    else
        let (start, stop) = normalizeArcDirection beam
        let angles = generateWrappedGantryAngles start stop step
        generateAnglePairs angles
        |> Array.map (fun (g1, g2) ->
            let a = angleToPoint (beam, g1)
            let b = angleToPoint (beam, g2)
            (a, b))

/// Normalize a segment (a, b) so that (a, b) ≡ (b, a) for deduplication.
let normalizeSegment (a: Vector3, b: Vector3) =
    if a < b then (a, b) else (b, a) // assumes Vector3 has comparison

/// Collects all unique (a, b) segments across all beams.
let extractAllSegments (beams: Beam[]) (step: float) : (Vector3 * Vector3)[] =
    beams
    |> Array.collect (fun beam -> generateArcSegments beam step)
    |> Array.map normalizeSegment
    |> Array.distinct


