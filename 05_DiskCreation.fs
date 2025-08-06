module VMS.TPS.DiskCreation

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.VectorMath
open VMS.TPS.BeamGeometry

[<Measure>]
type mm

// Attempts to define a disk at a given distance from isocenter along the line to the source
let defineLinacDiskFromSingleIsocenterAndSource
    (isocenter : VVector)
    (sourcePosition : VVector)
    (distance : float<mm>)
    (diskPrecision : int)
    =
    // Calculate direction from isocenter to source
    let dir =
        vnormalize (sourcePosition - isocenter)

    // Disk center is 'distance' away from isocenter along dir
    let diskCenter =
        isocenter
        + vscale dir (float distance)

    // Find two orthonormal vectors perpendicular to dir
    let up =
        if abs dir.z < 0.99 then
            VVector(0.0, 0.0, 1.0)
        else
            VVector(0.0, 1.0, 0.0)

    let v1 =
        vnormalize (vcross dir up)

    let v2 =
        vnormalize (vcross dir v1)

    let radius =
        390.0 // mm (39 cm)

    let perimeterPoints =
        [ 0 .. diskPrecision - 1 ]
        |> List.map (fun i ->
            let angle =
                2.0 * System.Math.PI * float i
                / float diskPrecision

            let offset =
                vscale v1 (radius * cos angle)
                + vscale v2 (radius * sin angle)

            diskCenter + offset)

    diskCenter :: perimeterPoints

let generateLinacDisksFromBeam
    (beam : Beam)
    (arcStep : float)
    (distance : float<mm>)
    (diskPrecision : int)
    =

    beam
    |> extractSourcePositions arcStep // (VVector * VVector) array
    |> Array.map (fun (isocenter, sourcePosition) ->
        defineLinacDiskFromSingleIsocenterAndSource
            isocenter
            sourcePosition
            distance
            diskPrecision
        |> List.toArray) // VVector[] for each disk
    |> ignore

    0
