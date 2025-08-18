(**
    Module: DiskCreation
    Purpose:
        - Defines circular collision disks along the beam line from isocenter to source,
        - generates disk samples for beams by stepping through source positions.
    Why:
        - Represents gantry clearance as reusable geometry primitives,
        - centralizes disk generation to keep scripts consistent.

    Notes:
        - Uses vector math and beam geometry helpers,
        - distances are in millimeters and precision is configurable.
*)

module VMS.TPS.DiskCreation

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.VectorMath
open VMS.TPS.BeamGeometry

[<Measure>]
type mm

/// Creates a circular disk orthogonal to the beam axis at a given offset from isocenter.
/// Uses the line from isocenter → source as the axis; the disk lies in the plane
/// perpendicular to that axis at the specified distance.
/// Returns the disk perimeter as a list of VVector points (counter‑clockwise).
let generateDiskOnBeamAxis
    (isocenter : VVector) // Center point of the plan/beam in DICOM coords
    (sourcePosition : VVector) // Radiation source position in DICOM coords
    (offset : float<mm>) // Distance from isocenter along axis toward the source (+) or away (−)
    (pointsPerDisk : int) // Number of points sampled on the disk perimeter
    : VVector list
    =
    // Calculate direction from isocenter to source
    let dir =
        vnormalize (sourcePosition - isocenter)

    // Disk center is 'distance' away from isocenter along dir
    let diskCenter =
        isocenter
        + vscale dir (float offset)

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
        [ 0 .. pointsPerDisk - 1 ]
        |> List.map (fun i ->
            let angle =
                2.0 * System.Math.PI * float i
                / float pointsPerDisk

            let offset =
                vscale v1 (radius * cos angle)
                + vscale v2 (radius * sin angle)

            diskCenter + offset)

    diskCenter :: perimeterPoints

/// Builds disks along a beam’s trajectory by sampling source/isocenter positions.
/// For each sampled control point (or arc step) the function creates a disk
/// via `generateDiskOnBeamAxis` and returns one array per step.
/// Returns an array where each element is a VVector[] representing one disk.
let generateDisksForBeam
    (beam : Beam) // Plan beam to sample
    (arcStep : float) // Step size in degrees for arc sampling; ignored for static beams
    (offset : float<mm>) // Distance from isocenter along the beam axis for each disk
    (pointsPerDisk : int) // Number of perimeter points per disk
    =
    beam
    |> extractSourcePositions arcStep // (VVector * VVector)[] of (isocenter, source)
    |> Array.map (fun (iso, src) ->
        generateDiskOnBeamAxis iso src offset pointsPerDisk
        |> List.toArray) // VVector[] for each disk
