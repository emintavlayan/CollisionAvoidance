namespace Shared

open System

type SamplingSettingsDto = {
    BodySampleStepMm: float option
    BeamSampleStepMm: float option
    ClearanceDistanceMm: float option
    CollisionToleranceMm: float option
}

type CollisionStatusDto =
    | AnalysisPending
    | NoCollision
    | CollisionDetected
    | AnalysisError of string

type CollisionPointDto = {
    Location: Point3D
    DistanceMm: float option
    Description: string option
}

type ControlPointCollisionResultDto = {
    ControlPointIndex: int
    Status: CollisionStatusDto
    CollisionPoints: CollisionPointDto list
}

type BeamCollisionResultDto = {
    BeamId: string
    Status: CollisionStatusDto
    ControlPointResults: ControlPointCollisionResultDto list
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
    BeamResults: BeamCollisionResultDto list
}
