module CollisionAvoidance.EsapiExporter.EsapiPlanExtraction

open Shared

/// Extracts a detached plan snapshot from the current ESAPI plan context.
let extractPlanSnapshot (_planContext: obj) : PlanSnapshotDto =
    failwith "TODO: Extract plan metadata and beam snapshots from ESAPI."

/// Extracts a detached beam snapshot from one ESAPI beam.
let extractBeamSnapshot (_beamContext: obj) : BeamSnapshotDto =
    failwith "TODO: Extract one beam into BeamSnapshotDto."

/// Extracts a detached control point snapshot from one ESAPI control point.
let extractControlPointSnapshot (_controlPointContext: obj) : ControlPointSnapshotDto =
    failwith "TODO: Extract one control point into ControlPointSnapshotDto."

/// Extracts a detached body snapshot from the ESAPI structure set and BODY structure.
let extractBodySnapshot (_structureSetContext: obj) (_bodyStructureContext: obj) : BodySnapshotDto =
    failwith "TODO: Extract BODY mesh and contour slices into BodySnapshotDto."

/// Extracts the full detached collision run request from the current ESAPI context.
let extractCollisionRunRequest (_planContext: obj) (_structureSetContext: obj) (_bodyStructureContext: obj) : CollisionRunRequestDto =
    failwith "TODO: Compose the full CollisionRunRequestDto from ESAPI."
