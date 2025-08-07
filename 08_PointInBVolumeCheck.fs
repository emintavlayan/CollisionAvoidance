(**
    Module: PointInBodyCheck
    Purpose:
        - Provides utilities to determine whether 3D points lie within a structure volume,
        - uses pre-extracted, thread-safe SnapshotVolume slices.

    Why:
        - ESAPI’s Structure.IsPointInsideSegment is not thread-safe,
        - enables robust, high-performance point-in-body checks in parallel workflows.

    Notes:
        - Assumes axial Z-slice segmentation of the volume,
        - Uses even-odd polygon rule for 2D inclusion test,
        - Ignores inner holes (only outer contours used for speed),
        - Includes fail-fast and batched modes for flexibility.
*)

module VMS.TPS.PointInBodyCheck

open StructureSnapshot
open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.VectorMath
open System

/// Calculates whether a 2D point is inside a polygon using the even-odd rule.
/// Uses a `mutable` accumulator (`inside`) for performance.
/// This imperative version is significantly faster than a pure one in parallel loops.
let pointInPolygon2D (x : float) (y : float) (poly : VVector[]) : bool =
    let mutable inside =
        false

    let n =
        poly.Length

    for i = 0 to n - 1 do
        let p1 =
            poly.[i]

        let p2 =
            poly.[(i + 1) % n]

        let crosses =
            (p1.y > y) <> (p2.y > y)
            && x < (p2.x - p1.x) * (y - p1.y)
                   / (p2.y - p1.y + 1e-12)
                   + p1.x

        if crosses then
            inside <- not inside

    inside

/// Finds the nearest slice within a tolerance
let findNearestSlice (slices : AxialSlice[]) (zTol : float) (z : float) =
    slices
    |> Array.tryFind (fun s -> abs (s.z - z) <= zTol)

/// Checks whether each point is inside the volume
let checkPointsInside
    (volume : SnapshotVolume)
    (points : VVector[])
    (zTol : float)
    : bool[]
    =
    points
    |> Array.Parallel.map (fun p ->
        match findNearestSlice volume.slices zTol p.z with
        | Some slice -> pointInPolygon2D p.x p.y slice.loop
        | None -> false)

/// Fail-fast version: returns true if ANY point is inside
let anyPointInside
    (volume : SnapshotVolume)
    (points : VVector[])
    (zTol : float)
    : bool
    =
    points
    |> Array.exists (fun p ->
        match findNearestSlice volume.slices zTol p.z with
        | Some slice -> pointInPolygon2D p.x p.y slice.loop
        | None -> false)
