module CollisionSceneBuilder

open Shared
open AccessoryVolume

/// Represents the BODY-plus-optional-structures composition used for server-side scene assembly.
type CollisionVolumeComposition = {
    Body: StructureSnapshotDto
    OptionalStructures: StructureSnapshotDto list
    Structures: StructureSnapshotDto array
}

/// Represents the detached collision scene passed into later visualization or accessory workflows.
type CollisionScene = {
    Body: BodySnapshotDto
    Beams: BeamSnapshotDto list
    Accessories: AccessoryModelDto list
}

/// Converts a BODY snapshot into the general detached structure shape used for composition.
let toStructureSnapshot (body: BodySnapshotDto) : StructureSnapshotDto = {
    StructureId = body.StructureId
    DisplayName = body.DisplayName
    Mesh = body.Mesh
    ContourSlices =
        body.ContourSlices
        |> List.map (fun slice -> {
            Z = slice.Z
            Contours = slice.Contours
            Bounds = slice.Bounds
        })
    Bounds = body.Bounds
}

/// Checks whether a detached structure should be treated as BODY during composition.
let isBodyStructure (structure: StructureSnapshotDto) =
    System.String.Equals(structure.StructureId, "BODY", System.StringComparison.OrdinalIgnoreCase)

/// Composes BODY plus optional detached structures from a list for first-version analysis.
let composeStructureVolumeList (structures: StructureSnapshotDto list) : Result<CollisionVolumeComposition, string> =
    match structures with
    | [] ->
        Error "Volume composition requires BODY plus optional structures, but the input list was empty."
    | _ ->
        match structures |> List.tryFind isBodyStructure with
        | None ->
            Error "Volume composition requires a BODY structure."
        | Some body ->
            let optionalStructures = structures |> List.filter (fun structure -> isBodyStructure structure |> not)

            Ok {
                Body = body
                OptionalStructures = optionalStructures
                Structures = structures |> List.toArray
            }

/// Composes BODY plus optional detached structures from an array for first-version analysis.
let composeStructureVolumeArray (structures: StructureSnapshotDto array) =
    structures |> Array.toList |> composeStructureVolumeList

/// Composes BODY plus optional accessory-derived structures from a detached collision request.
let composeRequestVolumes (request: CollisionRunRequestDto) =
    let bodyStructure = toStructureSnapshot request.Body
    let optionalStructures = request.Accessories |> getEnabledAccessoryStructures
    composeStructureVolumeList (bodyStructure :: optionalStructures)

/// Builds the detached collision scene used by later analysis and visualization boundaries.
let buildScene (request: CollisionRunRequestDto) = {
    Body = request.Body
    Beams = request.Plan.Beams
    Accessories = request.Accessories
}
