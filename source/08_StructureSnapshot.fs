(**
    Module: StructureSnapshot
    Purpose: 
        - Extracts ESAPI Structure into a simplified, thread-safe axial contour representation,
        - enabling fast point-in-body checks without depending on ESAPI threading constraints.

    Why:
        - ESAPI structures are not thread-safe, making them unsuitable for parallel workflows,
        - a precomputed snapshot allows multithreaded geometry checks across many test points,
        - enables exporting or persisting a lightweight volume representation.

    Notes:
        - Uses Structure.GetContoursOnImagePlane (safe and documented ESAPI API),
        - Ignores holes (inner contours) for performance,
        - Each slice contains the outer loop only and includes precomputed 2D bounding box,
        - The global 3D bounding box is computed manually for compatibility and full control.
*)

module VMS.TPS.StructureSnapshot

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types

/// 2D bounding box in the XY plane for a contour loop
type BoundingBox2D = {
    minX : float
    maxX : float
    minY : float
    maxY : float
}

/// Bounding box in 3D space across all slices
type BoundingBox3D = {
    min : VVector
    max : VVector
}

/// One axial slice of the structure (outer loop only, plus 2D bounds)
type AxialSlice = {
    z : float
    loop : VVector[]
    bounds : BoundingBox2D
}

/// Full thread-safe volume representation (with per-slice and global bounding boxes)
type SnapshotVolume = {
    slices : AxialSlice[]
    sliceThickness : float
    bounds : BoundingBox3D
}

/// Computes a 2D bounding box in the XY plane for a given contour loop.
/// This is used to quickly rule out points that lie outside the slice contour.
let computeBoundingBox2D (loop : VVector[]) : BoundingBox2D =
    let xs =
        loop
        |> Array.map (fun v -> v.x)

    let ys =
        loop
        |> Array.map (fun v -> v.y)

    {
        minX = Array.min xs
        maxX = Array.max xs
        minY = Array.min ys
        maxY = Array.max ys
    }

/// Computes the 3D bounding box that encloses all points across all slices.
/// Used as a fast global filter before performing expensive point-in-volume checks.
/// Avoids using ESAPI bounding box to keep the module self-contained and thread-safe.
let computeBoundingBox3D (slices : AxialSlice[]) : BoundingBox3D =
    let allPoints =
        slices
        |> Array.collect (fun s -> s.loop)

    let xs =
        allPoints
        |> Array.map (fun v -> v.x)

    let ys =
        allPoints
        |> Array.map (fun v -> v.y)

    let zs =
        slices
        |> Array.map (fun s -> s.z)

    {
        min = VVector(Array.min xs, Array.min ys, Array.min zs)
        max = VVector(Array.max xs, Array.max ys, Array.max zs)
    }

/// Extracts a thread-safe snapshot of a structure for parallel use
/// Includes a global 3D bounding box and per-slice 2D bounding boxes
let extractSnapshotVolume
    (ss : StructureSet)
    (structure : Structure)
    : SnapshotVolume
    =
    let image =
        ss.Image
        
    let sliceThickness =
        image.ZRes

    let zStart =
        image.Origin.z

    let zCount =
        image.ZSize

    let slices =
        [ 0 .. zCount - 1 ]
        |> List.choose (fun zIndex ->
            let contours =
                structure.GetContoursOnImagePlane(zIndex)

            if
                contours.Length > 0
                && contours.[0].Length > 0
            then
                let z =
                    zStart
                    + float zIndex * sliceThickness

                let outer =
                    [|0 .. contours.Length - 1|]
                    |> Array.map(fun i -> contours.[i]) // ignore holes (inner contours)
                    |> Array.concat
                    
                let points =
                    outer
                    |> Array.map (fun pt -> VVector(pt.x, pt.y, pt.z))

                let bounds =
                    computeBoundingBox2D points

                Some {
                    z = z
                    loop = points
                    bounds = bounds
                }
            else
                None)
        |> List.toArray


    let bounds =
        computeBoundingBox3D slices

    {
        slices = slices
        sliceThickness = sliceThickness
        bounds = bounds
    }



// hull code lifted from internet to test convex hull 
let clockwise (p1 : VVector) (p2 : VVector) (p3 : VVector) =
    (p2.x - p1.x) * (p3.y - p1.y)
    - (p2.y - p1.y) * (p3.x - p1.x)
    <= 0.0

let rec chain (hull: VVector list) (candidates: VVector list) =
    match candidates with
    | [ ] -> hull
    | c :: rest ->
        match hull with
        | [ ] -> chain [ c ] rest
        | [ start ] -> chain [c ; start] rest
        | b :: a :: tail -> 
            if clockwise a b c then chain (c :: hull) rest else
            chain (a :: tail) candidates

let hull (points: VVector list) =
    match points with
    | [ ] -> points
    | [ _ ] -> points
    | _ ->
        let sorted = 
            points
            |> List.sortBy(fun p -> p.x, p.y)
        let upper = chain [ ] sorted
        let lower = chain [ ] (List.rev sorted)
        List.append (List.tail upper) (List.tail lower)


let findHullOfTwoSlices
    (slice1 : AxialSlice)
    (slice2 : AxialSlice)
    : AxialSlice
    = 
    let loopHull = 
        Array.concat [slice1.loop; slice2.loop]
        |> Array.toList
        |> hull
        |> List.toArray

    let boundsHull = computeBoundingBox2D loopHull

    {
        z = slice1.z
        loop = loopHull
        bounds = boundsHull
    }

let findHullOfTwoVolumes
    (volume1 : SnapshotVolume)
    (volume2 : SnapshotVolume)
    : SnapshotVolume
    =
    
    let slicesHull =
        (volume1.slices, volume2.slices)
        ||> Array.map2(fun s1 s2 -> findHullOfTwoSlices s1 s2)

    let boundsHull = computeBoundingBox3D slicesHull
    
    {
        slices = slicesHull
        sliceThickness = volume1.sliceThickness
        bounds = boundsHull
    }

let findHullOfSlice
    (slice : AxialSlice)
    : AxialSlice
    = 
    let loopHull = 
        slice.loop
        |> Array.toList
        |> hull
        |> List.toArray

    {
        z = slice.z
        loop = loopHull
        bounds = slice.bounds
    }

let findHullOfVolume (volume : SnapshotVolume) : SnapshotVolume =
    let slicesHull =
        volume.slices
        |> Array.map(findHullOfSlice)

    {
        slices = slicesHull
        sliceThickness = volume.sliceThickness
        bounds = volume.bounds
    }
