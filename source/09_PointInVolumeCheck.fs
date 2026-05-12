
module VMS.TPS.PointInVolumeCheck

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open FSharp.Collections.ParallelSeq
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath
open StructureSnapshot


/// Checks whether a point is inside the structure bounding box
let isInsideBoundingBoxOf (structure : Structure) (point : VVector) : bool =
    structure.MeshGeometry.Bounds.Contains(point.x, point.y, point.z)

/// Checks whether a point is inside the structure segment volume
let isInsideStructureVolumeOf (structure : Structure) (point : VVector) : bool =
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


   
/// Möller–Trumbore intersection algorithm
let MTIntersectionALgorithm
    (rayOrigin : VVector)
    (triangle : VVector list)
    : bool
    =
    let rayVector = VVector(0.0, 1.0, 0.0)

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
    let test =
        [0 .. structureMesh.TriangleIndices.Count/3 - 1]
        |> List.map(fun i -> [structureMesh.TriangleIndices[3 * i]; structureMesh.TriangleIndices[3 * i + 1]; structureMesh.TriangleIndices[3 * i + 2]])
        |> List.map(fun i -> [VVector(structureMesh.Positions[i[0]].X, structureMesh.Positions[i[0]].Y, structureMesh.Positions[i[0]].Z); VVector(structureMesh.Positions[i[1]].X, structureMesh.Positions[i[1]].Y, structureMesh.Positions[i[1]].Z); VVector(structureMesh.Positions[i[2]].X, structureMesh.Positions[i[2]].Y, structureMesh.Positions[i[2]].Z)])
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
            match hasCollisionWithStructureMesh structureMesh diskPoints with
            | true ->
                Error "Collision detected. At least one disk point is inside BODY."

            | false ->
                Ok "No collision detected. No disk point is inside BODY."
    }


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
    (zTol : float)
    (points : VVector[])
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
    (sliceSpacing : float)
    (points : VVector[])
    : bool
    =
    points
    |> Array.exists (fun p ->
        match findSliceForZ volume.slices sliceSpacing p.z with
        | Some slice -> isPointInPolygon2D p.x p.y slice.loop
        | None -> false)

let ispointInside
    (volume : SnapshotVolume)
    (sliceSpacing : float)
    (point : VVector)
    : bool
    =
    match findSliceForZ volume.slices sliceSpacing point.z with
    | Some slice -> isPointInPolygon2D point.x point.y slice.loop
    | None -> false





let hasCollisionWithStructureTest
    (volume : SnapshotVolume)
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : bool
    =
    
    
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let collision =
        diskPoints
        |> Seq.filter (isInsideBoundingBoxOfMesh structureMesh)
        |> Seq.exists (ispointInside volume 1.0)
    // Seq exists is Lazy : if it finds one it does not calculate other
    stopWatch.Stop()
    showMessageBox ("Collsision test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms")
    collision


let checkDiskPointsAgainstStructureTest
    (volume : SnapshotVolume)
    (structureMesh : System.Windows.Media.Media3D.MeshGeometry3D)
    (diskPoints : VVector list)
    : Result<string, string>
    =
  
    result {
        return!
            match hasCollisionWithStructureTest volume structureMesh diskPoints with
            | true ->
                Error "Collision detected. At least one disk point is inside BODY."

            | false ->
                Ok "No collision detected. No disk point is inside BODY."
    }