namespace Shared

open System

type SamplingSettingsDto = {
    BodySampleStepMm: float option
    BeamSampleStepMm: float option
    ClearanceDistanceMm: float option
    CollisionToleranceMm: float option
    ArcStepDegrees: float option
}

type CollisionStatusDto =
    | AnalysisPending
    | NoCollision
    | CollisionDetected
    | AnalysisError of string

type CollisionPointSampleTypeDto =
    | LineSample
    | FirstCapSample
    | LastCapSample
    | InteriorCapSample
    | MeshSample
    | OtherSample of string

type CollisionPointSourceDto = {
    BeamId: string
    ControlPointIndex: int option
    GantryAngle: float option
    SampleType: CollisionPointSampleTypeDto
}

type CollisionPointDto = {
    Location: Point3D
    DistanceMm: float option
    Description: string option
    Source: CollisionPointSourceDto option
}

type ControlPointCollisionResultDto = {
    ControlPointIndex: int
    GantryAngle: float option
    Status: CollisionStatusDto
    CollisionPoints: CollisionPointDto list
}

type BeamCollisionResultDto = {
    BeamId: string
    BeamName: string option
    Status: CollisionStatusDto
    ControlPointResults: ControlPointCollisionResultDto list
}

type FlatCollisionResultDto = {
    Status: CollisionStatusDto
    GeneratedPointCount: int
    BoundingBoxCandidateCount: int
    InsidePointCount: int
    ElapsedMs: float option
}

type DetailedCollisionResultDto = {
    Status: CollisionStatusDto
    BeamResults: BeamCollisionResultDto list
    ControlPointResults: ControlPointCollisionResultDto list
    CollisionPoints: CollisionPointDto list
}

type CollisionRunRequestDto = {
    Plan: PlanSnapshotDto
    Body: BodySnapshotDto
    SamplingSettings: SamplingSettingsDto
    Accessories: AccessoryModelDto list
}

type CollisionRunSummaryDto = {
    RunId: Guid
    CreatedAtUtc: DateTime
    Status: CollisionStatusDto
    FlatResult: FlatCollisionResultDto option
    DetailedResult: DetailedCollisionResultDto option
    BeamResults: BeamCollisionResultDto list
}
