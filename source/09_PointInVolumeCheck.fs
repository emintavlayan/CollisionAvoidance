
module VMS.TPS.PointInVolumeCheck

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open FSharp.Collections.ParallelSeq
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath
open StructureSnapshot




let isInsideBoundingBoxOfMesh (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D) (point : VVector) : bool =
    structureMesh.Bounds.Contains(point.x, point.y, point.z)



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

/// Finds the axial slice whose Z-slab contains the point.
/// Assumes slices are sorted by z and use uniform spacing (slice thickness).
let findSliceForZ (slices : AxialSlice[]) (spacing : float) (zPoint : float) =
    let half =
        spacing / 2.0

    slices
    |> Array.tryFind (fun s ->
        zPoint >= (s.z - half)
        && zPoint < (s.z + half))

/// Checks whether a point is inside the volume (fail-fast).
/// Uses slice spacing to select the corresponding slab.
let ispointInside
    (volume : SnapshotVolume)
    (point : VVector)
    : bool
    =
    match findSliceForZ volume.slices volume.sliceThickness point.z with
    | Some slice -> isPointInPolygon2D point.x point.y slice.loop
    | None -> false


let hasCollisionWithStructure
    (volume : SnapshotVolume)
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : bool
    =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> Seq.filter (isInsideBoundingBoxOfMesh structureMesh)
        |> Seq.exists (ispointInside volume)
    // Seq exists is Lazy : if it finds one it does not calculate other
    stopWatch.Stop()
    showMessageBox ("Collsision test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")
    collision

let hasCollisionWithStructureParallel
    (volume : SnapshotVolume)
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : bool
    =

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> PSeq.filter (isInsideBoundingBoxOfMesh structureMesh)
        |> PSeq.exists (ispointInside volume)
    // Seq exists is Lazy : if it finds one it does not calculate other
    stopWatch.Stop()
    showMessageBox ("Collsision test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")
    collision


let checkDiskPointsAgainstStructure
    (volume : SnapshotVolume)
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : Result<string, string>
    =
  
    result {
        return!
            match hasCollisionWithStructure volume structureMesh diskPoints with
            | true ->
                Error "Collision detected. At least one point is inside BODY."

            | false ->
                Ok "No collision detected. No point is inside BODY."
    }

    