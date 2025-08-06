module VMS.TPS.DiskCreation

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.VectorMath

[<Measure>]
type mm

type LinacHeadDisk = {
    Center: VVector
    PerimeterPoints: VVector list
}

// Attempts to define a disk at a given distance from isocenter along the line to the source
let defineLinacDiskFromIsoAndSource (isocenter: VVector) (distance: float<mm>) (diskPrecision: int) (sourcePosition: VVector) : LinacHeadDisk =
    // Calculate direction from isocenter to source
    let dir = vnormalize (sourcePosition - isocenter)
    // Disk center is 'distance' away from isocenter along dir
    let diskCenter = isocenter + vscale dir (float distance)

    // Find two orthonormal vectors perpendicular to dir
    let up =
        if abs dir.z < 0.99 then VVector(0.0, 0.0, 1.0) else VVector(0.0, 1.0, 0.0)
    let v1 = vnormalize (vcross dir up)
    let v2 = vnormalize (vcross dir v1)
    let radius = 390.0 // mm (39 cm)
    let perimeterPoints =
        [0 .. diskPrecision - 1]
        |> List.map (fun i ->
            let angle = 2.0 * System.Math.PI * float i / float diskPrecision
            let offset = vscale v1 (radius * cos angle) + vscale v2 (radius * sin angle)
            diskCenter + offset
        )
    { Center = diskCenter; PerimeterPoints = perimeterPoints }

let defineLinacDiskFromStaticBeam (beam: Beam) (distance: float<mm>) (diskPrecision: int) : LinacHeadDisk =
    let gantryAngle = beam.ControlPoints.[0].GantryAngle
    let isocenter = beam.IsocenterPosition
    let sourcePosition = beam.GetSourceLocation(gantryAngle)
    defineLinacDiskFromIsoAndSource isocenter distance diskPrecision sourcePosition

let defineLinacDisksFromArcBeam (beam: Beam) (distance: float<mm>) (diskPrecision: int) (arcStepPresicion: float) : LinacHeadDisk list =
    let controlPoints = beam.ControlPoints |> Seq.toList
    let gantryStart = controlPoints.Head.GantryAngle
    let gantryStop = controlPoints |> List.last |> fun cp -> cp.GantryAngle
    let isocenter = beam.IsocenterPosition
    let modulo360 (angle: float) =
        let a = angle % 360.0
        if a < 0.0 then a + 360.0 else a
    // Helper to generate the gantry angle sequence
    let generateAnglesCW start stop step =
        let rec loop acc current =
            let next = current + step
            if (start < stop && next > stop) || (start > stop && modulo360 next > modulo360 stop && modulo360 start < modulo360 stop) then acc @ [stop]
            else loop (acc @ [current]) next
        loop [] start
    let generateAnglesCCW start stop step =
        let rec loop acc current =
            let next = current - step
            if (start > stop && next < stop) || (start < stop && modulo360 next < modulo360 stop && modulo360 start > modulo360 stop) then acc @ [stop]
            else loop (acc @ [current]) next
        loop [] start
    let gantryAngles =
        match beam.GantryDirection with
        | GantryDirection.Clockwise ->
            let s = modulo360 gantryStart
            let e = modulo360 gantryStop
            if s = e then [s] // full arc
            else if s < e then [ for a in s .. arcStepPresicion .. e -> modulo360 a ]
            else [ for a in s .. arcStepPresicion .. (e + 360.0) -> modulo360 a ]
        | GantryDirection.CounterClockwise ->
            let s = modulo360 gantryStart
            let e = modulo360 gantryStop
            if s = e then [s] // full arc
            else if s > e then [ for a in s .. -arcStepPresicion .. e -> modulo360 a ]
            else [ for a in s .. -arcStepPresicion .. (e - 360.0) -> modulo360 a ]
        | _ -> failwith "Not an arc beam"
    let sourcePositions = gantryAngles |> List.map (fun angle -> beam.GetSourceLocation(angle))
    sourcePositions |> List.map (defineLinacDiskFromIsoAndSource isocenter distance diskPrecision)

let defineLinacDisksFromBeam (beam: Beam) (distance: float<mm>) (diskPrecision: int) (arcDivisionDegree: float) : LinacHeadDisk list =
    match beam.GantryDirection with
    | GantryDirection.None -> [defineLinacDiskFromStaticBeam beam distance diskPrecision]
    | GantryDirection.Clockwise | GantryDirection.CounterClockwise -> defineLinacDisksFromArcBeam beam distance diskPrecision arcDivisionDegree
    | _ -> failwith "oh shit"