(**
    Module: PointInVolumeCheck
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

module VMS.TPS.PointInVolumeCheck

open StructureSnapshot
open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.VectorMath
open System

/// Performs horizontal Ray casting 2D point-in-polygon test.
/// Uses a `mutable` accumulator (`inside`) for performance.
/// This imperative version is significantly faster than a pure one in parallel loops.
let isPointInPolygon2D (x : float) (y : float) (polygon : VVector[]) : bool =
    let mutable inside =
        false

    let n =
        polygon.Length

    for i = 0 to n - 1 do
        let p1 =
            polygon.[i]

        let p2 =
            polygon.[(i + 1) % n]

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
        | Some slice -> isPointInPolygon2D p.x p.y slice.loop
        | None -> false)


/// Finds the axial slice whose Z-slab contains the point.
/// Assumes slices are sorted by z and use uniform spacing (slice thickness).
let findSliceForZ (slices : AxialSlice[]) (spacing : float) (zPoint : float) =
    let half =
        spacing / 2.0

    slices
    |> Array.tryFind (fun s ->
        zPoint >= (s.z - half)
        && zPoint < (s.z + half))

/// Checks whether any point is inside the volume (fail-fast).
/// Uses slice spacing to select the corresponding slab.
let anyPointInside2
    (volume : SnapshotVolume)
    (points : VVector[])
    (sliceSpacing : float)
    : bool
    =
    points
    |> Array.exists (fun p ->
        match findSliceForZ volume.slices sliceSpacing p.z with
        | Some slice -> isPointInPolygon2D p.x p.y slice.loop
        | None -> false)
