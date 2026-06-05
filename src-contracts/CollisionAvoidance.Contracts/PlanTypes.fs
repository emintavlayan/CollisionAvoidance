namespace Shared

/// Represents the detached clinical beam classification used across ESAPI and SAFE.
type BeamKindDto =
    | TreatmentBeam
    | SetupField
    | ImagingField
    | OtherBeam of string

/// Represents the detached gantry rotation direction used for beam sampling.
type GantryDirectionDto =
    | Clockwise
    | CounterClockwise
    | NotSpecified

/// Represents one detached control-point snapshot extracted from a treatment beam.
type ControlPointSnapshotDto = {
    Index: int
    GantryAngle: float
    CouchAngle: float option
    CollimatorAngle: float option
    SourcePosition: Point3D option
    Isocenter: Point3D option
    MetersetWeight: float option
    PatientSupportAngle: float option
}

/// Represents one detached beam snapshot extracted from a plan.
type BeamSnapshotDto = {
    BeamId: string
    BeamName: string option
    BeamKind: BeamKindDto
    IsTreatmentBeam: bool
    IsSetupField: bool
    GantryDirection: GantryDirectionDto
    GantryStart: float option
    GantryStop: float option
    CouchAngle: float option
    PatientSupportAngle: float option
    CollimatorAngle: float option
    Isocenter: Point3D option
    SourcePosition: Point3D option
    ControlPoints: ControlPointSnapshotDto list
}

/// Represents one detached plan snapshot that SAFE can analyze without live ESAPI access.
type PlanSnapshotDto = {
    PatientId: string
    CourseId: string option
    StructureSetId: string option
    PlanId: string
    PlanName: string option
    Beams: BeamSnapshotDto list
}
