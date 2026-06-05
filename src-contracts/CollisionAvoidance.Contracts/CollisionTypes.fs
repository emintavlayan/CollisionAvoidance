namespace Shared

open System

/// Represents the detached sampling and tolerance settings used by SAFE collision analysis.
type SamplingSettingsDto = {
    BodySampleStepMm: float<mm> option
    BeamSampleStepMm: float<mm> option
    BeamAxisOffsetMm: float<mm> option
    ClearanceRadiusMm: float<mm> option
    ClearanceDistanceMm: float<mm> option
    CollisionToleranceMm: float<mm> option
    ArcStepDegrees: float option
}

/// Represents the high-level status of one collision-analysis stage or result.
type CollisionStatusDto =
    | AnalysisPending
    | NoCollision
    | CollisionDetected
    | AnalysisError of string

/// Represents the provenance label for one generated or detected collision sample point.
type CollisionPointSampleTypeDto =
    | LineSample
    | FirstCapSample
    | LastCapSample
    | InteriorCapSample
    | MeshSample
    | OtherSample of string

/// Represents the detached provenance attached to one collision point.
type CollisionPointSourceDto = {
    BeamId: string
    ControlPointIndex: int option
    GantryAngle: float option
    SampleType: CollisionPointSampleTypeDto
    AccessoryId: string option
}

/// Represents one detached collision point with optional measured distance and provenance.
type CollisionPointDto = {
    Location: Point3D
    DistanceMm: float<mm> option
    Description: string option
    Source: CollisionPointSourceDto option
}

/// Represents one detached control-point collision result.
type ControlPointCollisionResultDto = {
    ControlPointIndex: int
    GantryAngle: float option
    Status: CollisionStatusDto
    CollisionPoints: CollisionPointDto list
}

/// Represents one detached beam-level collision result.
type BeamCollisionResultDto = {
    BeamId: string
    BeamName: string option
    Status: CollisionStatusDto
    ControlPointResults: ControlPointCollisionResultDto list
}

/// Represents the fast flat collision-analysis result used for first-pass triage.
type FlatCollisionResultDto = {
    Status: CollisionStatusDto
    GeneratedPointCount: int
    BoundingBoxCandidateCount: int
    InsidePointCount: int
    ElapsedMs: float option
}

/// Represents the detailed collision-analysis result with grouped provenance.
type DetailedCollisionResultDto = {
    Status: CollisionStatusDto
    BeamResults: BeamCollisionResultDto list
    ControlPointResults: ControlPointCollisionResultDto list
    CandidatePoints: CollisionPointDto list option
    CollisionPoints: CollisionPointDto list
}

/// Represents one detached collision-analysis request sent from ESAPI to SAFE.
type CollisionRunRequestDto = {
    Plan: PlanSnapshotDto
    Body: BodySnapshotDto
    SamplingSettings: SamplingSettingsDto
    Accessories: AccessoryModelDto list
}

/// Represents the stored summary for one collision-analysis run.
type CollisionRunSummaryDto = {
    RunId: Guid
    CreatedAtUtc: DateTime
    Status: CollisionStatusDto
    FlatResult: FlatCollisionResultDto option
    DetailedResult: DetailedCollisionResultDto option
    BeamResults: BeamCollisionResultDto list
}

/// Represents the SAFE response returned after creating one collision-analysis run.
type CreateCollisionRunResponseDto = {
    RunId: Guid
    RunUrl: string option
    Summary: CollisionRunSummaryDto
}

/// Represents the stored request-plus-summary payload for one collision-analysis run.
type CollisionRunDetailsDto = {
    RunId: Guid
    Request: CollisionRunRequestDto
    Summary: CollisionRunSummaryDto
}
