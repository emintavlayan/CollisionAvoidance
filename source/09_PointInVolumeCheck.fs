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

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open FSharp.Collections.ParallelSeq
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath


/// Checks whether a point is inside the structure bounding box
let isInsideBoundingBoxOf (structure : Structure) (point : VVector) : bool =
    structure.MeshGeometry.Bounds.Contains(point.x, point.y, point.z)

/// Checks whether a point is inside the structure segment volume
let isInsideStructureVolumeOf (structure : Structure) (point : VVector) : bool =
    showMessageBox "checking point against body structure"
    structure.IsPointInsideSegment point

/// Checks whether any disk point collides with the structure
let hasCollisionWithStructure
    (structure : Structure)
    (diskPoints : VVector list)
    : bool
    =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> Seq.filter (isInsideBoundingBoxOf structure)
        |> Seq.exists (isInsideStructureVolumeOf structure)
    // Seq exists is Lazy : if it finds one it does not calculate other
    stopWatch.Stop()
    showMessageBox ("Collsision test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")

    collision
    
    

/// Parallelized version of has CollisionWithStructure for checking if a point is inside the structure segment volume
let hasCollisionWithStructureParallel
    (structure : Structure)
    (diskPoints : VVector list)
    : bool
    =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> PSeq.filter (isInsideBoundingBoxOf structure)
        |> PSeq.exists (isInsideStructureVolumeOf structure)
    stopWatch.Stop()
    showMessageBox ("Parallel collsision  test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")
    
    collision

/// Checks disk points against a structure and returns collision status
let checkDiskPointsAgainstStructure
    (structure : Structure)
    (diskPoints : VVector list)
    : Result<string, string>
    =
        
    result {
        return!
            match hasCollisionWithStructure structure diskPoints with
            | true ->
                Error "Collision detected. At least one disk point is inside BODY."

            | false ->
                Ok "No collision detected. No disk point is inside BODY."
    }


    (*
    finish
    ensure <> are set correctly
    adapt to better suit inputs from mesh
    *)
/// Möller–Trumbore intersection algorithm
let MTIntersectionALgorithm
    (rayOrigin : VVector)
    (triangle : VVector list)
    : bool
    =
    let rayVector = VVector(1.0, 0.0, 0.0)

    let epsilon = 1e-10
    let edge1 = triangle[1] - triangle[0] 
    let edge2 = triangle[2] - triangle[0] 

    let rayCrossE2 = vcross rayVector edge2
    let det = vdot edge1 rayCrossE2
    if (abs(det) < epsilon) then
        false
    else

    let invDet = 1.0 / det
    let s = rayOrigin - triangle[0]
    let u = invDet * vdot s rayCrossE2
    if (u < -epsilon || u > 1.0) then
        false
    else

    let sCrossE1 = vcross s edge1
    let v = invDet * vdot rayVector sCrossE1
    
    if(v < 0.0 || u + v > 1.0 ) then 
        false
    else
        true


let isInsideBoundingBoxOfMesh (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D) (point : VVector) : bool =
    structureMesh.Bounds.Contains(point.x, point.y, point.z)

/// Checks whether a point is inside the structure segment volume
let isInsideStructureVolumeOfMesh (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D) (point : VVector) : bool =
    showMessageBox "checking point against body structure"

    let test =
        [0 .. structureMesh.TriangleIndices.Count/3]
        //|> List.toSeq
        |> List.map(fun i -> [VVector(structureMesh.Positions[i * 3].X, structureMesh.Positions[i * 3].Y, structureMesh.Positions[i * 3].Z); VVector(structureMesh.Positions[i * 3 + 1].X, structureMesh.Positions[i * 3 + 1].Y, structureMesh.Positions[i * 3 + 1].Z); VVector(structureMesh.Positions[i * 3 + 2].X, structureMesh.Positions[i * 3 + 2].Y, structureMesh.Positions[i * 3 + 2].Z)])
        |> List.filter(MTIntersectionALgorithm point)
        |> List.length

    if test % 2 <> 0 then
        true
    else
        false

let hasCollisionWithStructureMesh
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : bool
    =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> Seq.filter (isInsideBoundingBoxOfMesh structureMesh)
        |> Seq.exists (isInsideStructureVolumeOfMesh structureMesh)
    // Seq exists is Lazy : if it finds one it does not calculate other
    stopWatch.Stop()
    showMessageBox ("Collsision test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")

    collision

let hasCollisionWithStructureParallelMesh
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : bool
    =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> PSeq.filter (isInsideBoundingBoxOfMesh structureMesh)
        |> PSeq.exists (isInsideStructureVolumeOfMesh structureMesh)
    stopWatch.Stop()
    showMessageBox ("Parallel collsision  test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")
    
    collision

let checkDiskPointsAgainstStructureMesh
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : Result<string, string>
    =
        
    result {
        return!
            match hasCollisionWithStructureParallelMesh structureMesh diskPoints with
            | true ->
                Error "Collision detected. At least one disk point is inside BODY."

            | false ->
                Ok "No collision detected. No disk point is inside BODY."
    }
