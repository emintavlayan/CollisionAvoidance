namespace Shared

type BeamKindDto =
    | TreatmentBeam
    | SetupField
    | ImagingField
    | OtherBeam of string

type GantryDirectionDto =
    | Clockwise
    | CounterClockwise
    | NotSpecified

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

type PlanSnapshotDto = {
    PatientId: string
    CourseId: string option
    StructureSetId: string option
    PlanId: string
    PlanName: string option
    Beams: BeamSnapshotDto list
}
