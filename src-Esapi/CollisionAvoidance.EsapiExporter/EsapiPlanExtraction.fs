module CollisionAvoidance.EsapiExporter.EsapiPlanExtraction

open FsToolkit.ErrorHandling
open Shared
open CollisionAvoidance.EsapiExporter.EsapiGeometryMapping

/// Represents a compile-safe detached control-point projection used by the exporter boundary.
type EsapiControlPointLike = {
    Index: int
    GantryAngle: float
    CouchAngle: float option
    PatientSupportAngle: float option
    CollimatorAngle: float option
    SourcePosition: VectorLike option
    Isocenter: VectorLike option
    MetersetWeight: float option
}

/// Represents a compile-safe detached beam projection used by the exporter boundary.
type EsapiBeamLike = {
    BeamId: string
    BeamName: string option
    IsSetupField: bool
    GantryDirection: GantryDirectionDto
    GantryStart: float option
    GantryStop: float option
    CouchAngle: float option
    PatientSupportAngle: float option
    CollimatorAngle: float option
    Isocenter: VectorLike option
    SourcePosition: VectorLike option
    ControlPoints: EsapiControlPointLike list
}

/// Represents a compile-safe detached plan projection used by the exporter boundary.
type EsapiPlanLike = {
    PatientId: string
    CourseId: string option
    StructureSetId: string option
    PlanId: string
    PlanName: string option
    Beams: EsapiBeamLike list
}

/// Represents a compile-safe detached structure projection used by the exporter boundary.
type EsapiStructureLike = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshGeometryLike option
    ContourSlices: ContourSliceLike list
    SliceThicknessMm: float<mm> option
}

/// Represents a compile-safe detached BODY projection used by the exporter boundary.
type EsapiBodyLike = EsapiStructureLike

/// Represents a compile-safe detached structure-set projection used by the exporter boundary.
type EsapiStructureSetLike = {
    StructureSetId: string option
    Structures: EsapiStructureLike list
}

/// Represents the compile-safe detached export payload assembled before DTO mapping.
type EsapiCollisionRunLike = {
    Plan: EsapiPlanLike
    Body: EsapiStructureLike
    SamplingSettings: SamplingSettingsDto
    Accessories: AccessoryModelDto list
}

/// Maps a setup-field flag into a detached beam kind.
let mapBeamKind (isSetupField: bool) =
    if isSetupField then
        SetupField
    else
        TreatmentBeam

/// Returns the first detached control point when one exists.
let tryGetFirstControlPoint (beamContext: EsapiBeamLike) =
    beamContext.ControlPoints |> List.tryHead

/// Returns the last detached control point when one exists.
let tryGetLastControlPoint (beamContext: EsapiBeamLike) =
    beamContext.ControlPoints |> List.tryLast

/// Resolves the first-version gantry start angle from the beam or its first control point.
let resolveGantryStart (beamContext: EsapiBeamLike) =
    beamContext.GantryStart
    |> Option.orElseWith (fun () -> beamContext |> tryGetFirstControlPoint |> Option.map (fun point -> point.GantryAngle))

/// Resolves the first-version gantry stop angle from the beam or its last control point.
let resolveGantryStop (beamContext: EsapiBeamLike) =
    beamContext.GantryStop
    |> Option.orElseWith (fun () -> beamContext |> tryGetLastControlPoint |> Option.map (fun point -> point.GantryAngle))

/// Resolves the detached beam couch angle from the beam or its first control point.
let resolveBeamCouchAngle (beamContext: EsapiBeamLike) =
    beamContext.CouchAngle
    |> Option.orElseWith (fun () -> beamContext |> tryGetFirstControlPoint |> Option.bind (fun point -> point.CouchAngle))

/// Resolves the detached beam patient-support angle from the beam or its first control point.
let resolveBeamPatientSupportAngle (beamContext: EsapiBeamLike) =
    beamContext.PatientSupportAngle
    |> Option.orElseWith (fun () -> beamContext |> tryGetFirstControlPoint |> Option.bind (fun point -> point.PatientSupportAngle))

/// Resolves the detached beam collimator angle from the beam or its first control point.
let resolveBeamCollimatorAngle (beamContext: EsapiBeamLike) =
    beamContext.CollimatorAngle
    |> Option.orElseWith (fun () -> beamContext |> tryGetFirstControlPoint |> Option.bind (fun point -> point.CollimatorAngle))

/// Resolves the detached beam isocenter from the beam or its first control point.
let resolveBeamIsocenter (beamContext: EsapiBeamLike) =
    beamContext.Isocenter
    |> Option.map mapVVectorToPoint3D
    |> Option.orElseWith (fun () -> beamContext |> tryGetFirstControlPoint |> Option.bind (fun point -> point.Isocenter |> Option.map mapVVectorToPoint3D))

/// Resolves the detached beam source position from the beam or its first control point.
let resolveBeamSourcePosition (beamContext: EsapiBeamLike) =
    beamContext.SourcePosition
    |> Option.map mapVVectorToPoint3D
    |> Option.orElseWith (fun () -> beamContext |> tryGetFirstControlPoint |> Option.bind (fun point -> point.SourcePosition |> Option.map mapVVectorToPoint3D))

/// Maps detached BODY slices from an ESAPI structure projection.
let mapBodyContourSlices (structureContext: EsapiStructureLike) =
    structureContext.ContourSlices |> List.map mapBodySlice

/// Maps detached structure contour slices from an ESAPI structure projection.
let mapStructureContourSlices (structureContext: EsapiStructureLike) =
    structureContext.ContourSlices |> List.map mapContourSlice

/// Resolves detached BODY or structure bounds from mesh bounds first, then contour slices.
let resolveStructureBounds (mesh: MeshGeometryLike option) (bodySlices: BodySliceDto list) =
    mesh
    |> Option.bind (fun meshGeometry -> meshGeometry.Bounds)
    |> Option.map mapMeshBoundsToBounds3D
    |> Option.orElseWith (fun () -> tryCreateBounds3DFromSlices bodySlices)

/// Extracts a detached control point snapshot from one ESAPI control point projection.
let extractControlPointSnapshot (controlPointContext: EsapiControlPointLike) : ControlPointSnapshotDto = {
    Index = controlPointContext.Index
    GantryAngle = controlPointContext.GantryAngle
    CouchAngle = controlPointContext.CouchAngle
    CollimatorAngle = controlPointContext.CollimatorAngle
    SourcePosition = controlPointContext.SourcePosition |> Option.map mapVVectorToPoint3D
    Isocenter = controlPointContext.Isocenter |> Option.map mapVVectorToPoint3D
    MetersetWeight = controlPointContext.MetersetWeight
    PatientSupportAngle = controlPointContext.PatientSupportAngle
}

/// Extracts a detached beam snapshot from one ESAPI beam projection.
let extractBeamSnapshot (beamContext: EsapiBeamLike) : BeamSnapshotDto = {
    BeamId = beamContext.BeamId
    BeamName = beamContext.BeamName
    BeamKind = mapBeamKind beamContext.IsSetupField
    IsTreatmentBeam = not beamContext.IsSetupField
    IsSetupField = beamContext.IsSetupField
    GantryDirection = beamContext.GantryDirection
    GantryStart = resolveGantryStart beamContext
    GantryStop = resolveGantryStop beamContext
    CouchAngle = resolveBeamCouchAngle beamContext
    PatientSupportAngle = resolveBeamPatientSupportAngle beamContext
    CollimatorAngle = resolveBeamCollimatorAngle beamContext
    Isocenter = resolveBeamIsocenter beamContext
    SourcePosition = resolveBeamSourcePosition beamContext
    ControlPoints = beamContext.ControlPoints |> List.map extractControlPointSnapshot
}

/// Extracts a detached plan snapshot from the current ESAPI plan projection.
let extractPlanSnapshot (planContext: EsapiPlanLike) : PlanSnapshotDto = {
    PatientId = planContext.PatientId
    CourseId = planContext.CourseId
    StructureSetId = planContext.StructureSetId
    PlanId = planContext.PlanId
    PlanName = planContext.PlanName
    Beams = planContext.Beams |> List.map extractBeamSnapshot
}

/// Extracts detached BODY contour slices from one ESAPI structure projection.
let extractContourSlices (structureContext: EsapiStructureLike) : Result<BodySliceDto list, string> =
    let slices = mapBodyContourSlices structureContext

    if slices.IsEmpty then
        Error "BODY structure does not contain contour slices for first-version analysis."
    else
        Ok slices

/// Extracts a detached body snapshot from the ESAPI BODY projection.
let extractBodySnapshot (bodyContext: EsapiStructureLike) : Result<BodySnapshotDto, string> =
    result {
        let! contourSlices = extractContourSlices bodyContext

        let! mesh =
            match bodyContext.Mesh with
            | Some meshGeometry ->
                meshGeometry
                |> createDetachedMeshSnapshot
                |> Result.bind (fun snapshot -> snapshot |> getDetachedMeshValue |> mapMeshToMeshDto |> Result.map Some)
            | None -> Ok None

        return {
            StructureId = bodyContext.StructureId
            DisplayName = bodyContext.DisplayName
            Mesh = mesh
            ContourSlices = contourSlices
            Bounds = resolveStructureBounds bodyContext.Mesh contourSlices
            SliceThicknessMm = bodyContext.SliceThicknessMm
        }
    }

/// Extracts a detached structure snapshot from one ESAPI structure projection.
let extractStructureSnapshot (structureContext: EsapiStructureLike) : Result<StructureSnapshotDto, string> =
    result {
        let! mesh =
            match structureContext.Mesh with
            | Some meshGeometry ->
                meshGeometry
                |> createDetachedMeshSnapshot
                |> Result.bind (fun snapshot -> snapshot |> getDetachedMeshValue |> mapMeshToMeshDto |> Result.map Some)
            | None -> Ok None

        let contourSlices = mapStructureContourSlices structureContext
        let bodyLikeSlices = contourSlices |> List.map (fun slice -> { Z = slice.Z; Contours = slice.Contours; Bounds = slice.Bounds })

        return {
            StructureId = structureContext.StructureId
            DisplayName = structureContext.DisplayName
            Mesh = mesh
            ContourSlices = contourSlices
            Bounds = resolveStructureBounds structureContext.Mesh bodyLikeSlices
        }
    }

/// Extracts a detached accessory model from one ESAPI structure projection.
let extractAccessoryModel (kind: AccessoryKindDto) (structureContext: EsapiStructureLike) : Result<AccessoryModelDto, string> =
    extractStructureSnapshot structureContext
    |> Result.map (fun structure -> {
        AccessoryId = structure.StructureId
        Kind = kind
        DisplayName = structure.DisplayName |> Option.defaultValue structure.StructureId
        Mesh = structure.Mesh
        Structure = Some structure
        Bounds = structure.Bounds
        Offset = None
        IsEnabled = true
    })

/// Extracts the full detached collision run request from ESAPI-like plan and body projections.
let extractCollisionRunRequest (runContext: EsapiCollisionRunLike) : Result<CollisionRunRequestDto, string> =
    result {
        let! body = extractBodySnapshot runContext.Body

        return {
            Plan = extractPlanSnapshot runContext.Plan
            Body = body
            SamplingSettings = runContext.SamplingSettings
            Accessories = runContext.Accessories
        }
    }
