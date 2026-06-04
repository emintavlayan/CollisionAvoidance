module BodyVolume

open Shared
open ClearanceSampling

/// Checks whether a point lies within a detached 3D bounds record.
let isPointInsideBounds (bounds: Bounds3D) (point: Point3D) =
    point.X >= bounds.Min.X
    && point.X <= bounds.Max.X
    && point.Y >= bounds.Min.Y
    && point.Y <= bounds.Max.Y
    && point.Z >= bounds.Min.Z
    && point.Z <= bounds.Max.Z

/// Filters candidate points by the detached BODY bounds when bounds are available.
let filterCandidatesByBounds (body: BodySnapshotDto) (candidates: CollisionPointCandidate list) =
    match body.Bounds with
    | Some bounds ->
        candidates
        |> List.filter (fun candidate -> isPointInsideBounds bounds candidate.Location)
    | None -> candidates

/// Performs a 2D ray-casting test against a contour polygon.
let isPointInsidePolygon2D (point: Point3D) (polygon: Point3D list) =
    let mutable inside = false
    let points = polygon |> List.toArray
    let pointCount = points.Length

    if pointCount < 3 then
        false
    else
        for index in 0 .. pointCount - 1 do
            let firstPoint = points[index]
            let secondPoint = points[(index + 1) % pointCount]

            let crosses =
                (firstPoint.Y > point.Y) <> (secondPoint.Y > point.Y)
                && point.X
                   < (secondPoint.X - firstPoint.X) * (point.Y - firstPoint.Y)
                     / (secondPoint.Y - firstPoint.Y + 1e-12)
                     + firstPoint.X

            if crosses then
                inside <- not inside

        inside

/// Estimates the axial slice thickness for the detached BODY snapshot.
let estimateSliceThicknessMm (body: BodySnapshotDto) =
    match body.SliceThicknessMm with
    | Some thickness -> thickness
    | None ->
        match body.ContourSlices with
        | firstSlice :: secondSlice :: _ -> abs (secondSlice.Z - firstSlice.Z)
        | _ -> 1.0

/// Finds the detached BODY slice whose axial slab contains the point.
let findSliceForZ (body: BodySnapshotDto) (point: Point3D) =
    let halfThickness = estimateSliceThicknessMm body / 2.0

    body.ContourSlices
    |> List.tryFind (fun slice -> point.Z >= slice.Z - halfThickness && point.Z < slice.Z + halfThickness)

/// Checks whether a point lies inside any contour on one detached BODY slice.
let isPointInsideSlice (slice: BodySliceDto) (point: Point3D) =
    slice.Contours
    |> List.exists (fun contour -> isPointInsidePolygon2D point contour.Points)

/// Checks whether a point lies inside the detached BODY contour volume.
let isPointInsideBody (body: BodySnapshotDto) (point: Point3D) =
    match findSliceForZ body point with
    | Some slice -> isPointInsideSlice slice point
    | None -> false

/// Filters candidate points to those that are inside the detached BODY contour volume.
let filterCandidatesInsideBody (body: BodySnapshotDto) (candidates: CollisionPointCandidate list) =
    candidates
    |> List.filter (fun candidate -> isPointInsideBody body candidate.Location)
