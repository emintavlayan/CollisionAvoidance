# Shared domain

`src/Shared` defines the contracts that both the client and server use.

## Geometry

Files:

- `GeometryTypes.fs`
- `StructureTypes.fs`

Main concepts:

- `Point3D`, `Vector3D`, and `Bounds3D` represent spatial data.
- `MeshDto` carries triangle-mesh geometry.
- `ContourSliceDto` carries contour-based body geometry.
- `BodySnapshotDto` wraps patient body data with optional mesh and bounds.

## Plan data

File:

- `PlanTypes.fs`

Main concepts:

- `ControlPointSnapshotDto` represents one sampled machine state.
- `BeamSnapshotDto` groups control points and beam-level setup.
- `PlanSnapshotDto` is the root plan payload for a collision run.

## Accessories

File:

- `AccessoryTypes.fs`

Main concepts:

- `AccessoryKindDto` categorizes couch and setup accessories.
- `AccessoryModelDto` holds geometry, offsets, enablement, and identity.
- The specific `CouchBaseDto`, `VacFixDto`, and `BreastBoardDto` wrappers are thin typed views over `AccessoryModelDto`.

## Collision results

File:

- `CollisionTypes.fs`

Main concepts:

- `SamplingSettingsDto` defines how collision sampling should be performed.
- `CollisionStatusDto` tracks pending, success, collision, or error states.
- `CollisionPointDto` holds a single detected point.
- `ControlPointCollisionResultDto`, `BeamCollisionResultDto`, and `CollisionRunSummaryDto` define nested result aggregation.
- `CollisionRunRequestDto` is the root request contract for a future end-to-end analysis run.

## Legacy template model

File:

- `Shared.fs`

This file still contains the SAFE template todo model and `ITodosApi`. That code is separate from the collision DTOs and is currently what the live server/client flow still uses.
