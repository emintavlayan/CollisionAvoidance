module CollisionAvoidance.EsapiExporter.EsapiPlanExtraction

open FsToolkit.ErrorHandling
open Shared
open CollisionAvoidance.EsapiExporter.EsapiGeometryMapping

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

type EsapiPlanLike = {
    PatientId: string
    CourseId: string option
    StructureSetId: string option
    PlanId: string
    PlanName: string option
    Beams: EsapiBeamLike list
}

type EsapiBodyLike = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshGeometryLike option
    ContourSlices: ContourSliceLike list
    SliceThicknessMm: float option
}

type EsapiCollisionRunLike = {
    Plan: EsapiPlanLike
    Body: EsapiBodyLike
    SamplingSettings: SamplingSettingsDto
    Accessories: AccessoryModelDto list
}

/// Maps a setup-field flag into a detached beam kind.
let mapBeamKind (isSetupField: bool) =
    if isSetupField then
        SetupField
    else
        TreatmentBeam

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
    GantryStart = beamContext.GantryStart
    GantryStop = beamContext.GantryStop
    CouchAngle =
        beamContext.CouchAngle
        |> Option.orElseWith (fun () -> beamContext.ControlPoints |> List.tryHead |> Option.bind (fun point -> point.CouchAngle))
    PatientSupportAngle =
        beamContext.PatientSupportAngle
        |> Option.orElseWith (fun () -> beamContext.ControlPoints |> List.tryHead |> Option.bind (fun point -> point.PatientSupportAngle))
    CollimatorAngle =
        beamContext.CollimatorAngle
        |> Option.orElseWith (fun () -> beamContext.ControlPoints |> List.tryHead |> Option.bind (fun point -> point.CollimatorAngle))
    Isocenter =
        beamContext.Isocenter
        |> Option.map mapVVectorToPoint3D
        |> Option.orElseWith (fun () -> beamContext.ControlPoints |> List.tryHead |> Option.bind (fun point -> point.Isocenter |> Option.map mapVVectorToPoint3D))
    SourcePosition =
        beamContext.SourcePosition
        |> Option.map mapVVectorToPoint3D
        |> Option.orElseWith (fun () -> beamContext.ControlPoints |> List.tryHead |> Option.bind (fun point -> point.SourcePosition |> Option.map mapVVectorToPoint3D))
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

/// Extracts detached contour slices from the ESAPI body projection.
let extractContourSlices (bodyContext: EsapiBodyLike) : BodySliceDto list =
    bodyContext.ContourSlices |> List.map mapBodySlice

/// Extracts a detached body snapshot from the ESAPI BODY projection.
let extractBodySnapshot (bodyContext: EsapiBodyLike) : Result<BodySnapshotDto, string> =
    result {
        let! mesh =
            match bodyContext.Mesh with
            | Some meshGeometry ->
                meshGeometry
                |> createDetachedMeshSnapshot
                |> Result.bind (fun snapshot -> snapshot |> getDetachedMeshValue |> mapMeshToMeshDto |> Result.map Some)
            | None -> Ok None

        let bounds =
            bodyContext.Mesh
            |> Option.bind (fun meshGeometry -> meshGeometry.Bounds)
            |> Option.map mapMeshBoundsToBounds3D

        return {
            StructureId = bodyContext.StructureId
            DisplayName = bodyContext.DisplayName
            Mesh = mesh
            ContourSlices = extractContourSlices bodyContext
            Bounds = bounds
            SliceThicknessMm = bodyContext.SliceThicknessMm
        }
    }

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
