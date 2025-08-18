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
                    contours.[0] // ignore holes (inner contours)

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
