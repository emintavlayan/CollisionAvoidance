module CollisionSceneBuilder

open Shared

type CollisionScene = {
    Body: BodySnapshotDto
    Beams: BeamSnapshotDto list
    Accessories: AccessoryModelDto list
}

let buildScene (request: CollisionRunRequestDto) = {
    Body = request.Body
    Beams = request.Plan.Beams
    Accessories = request.Accessories
}
