module VMS.TPS.PointInVolumeCheck

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling

/// Checks whether a point is inside the structure bounding box
let isInsideBoundingBoxOf (structure : Structure) (point : VVector) : bool =
    let bounds =
        structure.MeshGeometry.Bounds

    point.x >= bounds.X
    && point.x <= bounds.X + bounds.SizeX
    && point.y >= bounds.Y
    && point.y <= bounds.Y + bounds.SizeY
    && point.z >= bounds.Z
    && point.z <= bounds.Z + bounds.SizeZ

/// Checks whether a point is inside the structure segment volume
let isInsideStructureVolumeOf (structure : Structure) (point : VVector) : bool =
    structure.IsPointInsideSegment point

/// Checks whether any disk point collides with the structure
let hasCollisionWithStructure
    (structure : Structure)
    (diskPoints : VVector list)
    : bool
    =

    diskPoints
    |> Seq.filter (isInsideBoundingBoxOf structure)
    |> Seq.exists (isInsideStructureVolumeOf structure)

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