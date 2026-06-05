module CollisionAvoidance.EsapiExporter.EsapiPlanExtraction

open System
open FsToolkit.ErrorHandling
open Shared
open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open CollisionAvoidance.EsapiExporter.EsapiGeometryMapping

/// Represents one real ESAPI gantry direction mapped into the detached contracts enum.
let mapGantryDirection (direction: GantryDirection) =
    match direction with
    | GantryDirection.Clockwise -> Clockwise
    | GantryDirection.CounterClockwise -> CounterClockwise
    | GantryDirection.None -> NotSpecified
    | _ -> NotSpecified

/// Represents one real ESAPI setup-field flag mapped into the detached beam-kind enum.
let mapBeamKind (isSetupField: bool) =
    if isSetupField then SetupField else TreatmentBeam

/// Represents one beam control-point list converted into a stable F# list.
let getControlPoints (beam: Beam) =
    beam.ControlPoints |> Seq.toList

/// Represents the first control point of a beam when one exists.
let tryGetFirstControlPoint (beam: Beam) =
    beam |> getControlPoints |> List.tryHead

/// Represents the last control point of a beam when one exists.
let tryGetLastControlPoint (beam: Beam) =
    beam |> getControlPoints |> List.tryLast

/// Represents the detached source position for a beam at the supplied gantry angle.
let tryGetSourcePosition (beam: Beam) (gantryAngle: float) : Point3D option =
    try
        beam.GetSourceLocation gantryAngle
        |> mapVVectorToPoint3D
        |> Some
    with _ ->
        None

/// Represents the detached patient identifier extracted from the current ESAPI patient.
let getPatientIdentifier (patient: Patient) =
    patient.Id2

/// Represents the detached display-name option for one nullable ESAPI string value.
let toOptionalText (value: string) =
    if String.IsNullOrWhiteSpace value then None else Some value

/// Represents the detached body or structure slice thickness extracted from the ESAPI image.
let getImageSliceThickness (structureSet: StructureSet) =
    if isNull structureSet.Image then None else Some (Length.millimeters structureSet.Image.ZRes)

/// Represents the detached Z coordinate for one image plane in the current structure set.
let getPlaneZ (structureSet: StructureSet) (zIndex: int) =
    structureSet.Image.Origin.z + float zIndex * structureSet.Image.ZRes

/// Represents the detached contour slices extracted from `Structure.GetContoursOnImagePlane`.
let extractContourSlicesOnImagePlanes (structureSet: StructureSet) (structure: Structure) =
    if isNull structureSet.Image then
        Error "Structure set image was not available."
    else
        let slices =
            [ 0 .. structureSet.Image.ZSize - 1 ]
            |> List.choose (fun zIndex ->
                let contours =
                    structure.GetContoursOnImagePlane zIndex
                    |> Array.filter (fun contour -> not (isNull contour) && contour.Length > 0)

                if contours.Length = 0 then
                    None
                else
                    Some (getPlaneZ structureSet zIndex, contours))

        Ok slices

/// Represents one detached control-point snapshot extracted from a real ESAPI control point.
let extractControlPointSnapshot (beam: Beam) (index: int) (controlPoint: ControlPoint) : ControlPointSnapshotDto = {
    Index = index
    GantryAngle = controlPoint.GantryAngle
    CouchAngle = Some controlPoint.PatientSupportAngle
    CollimatorAngle = Some controlPoint.CollimatorAngle
    SourcePosition = tryGetSourcePosition beam controlPoint.GantryAngle
    Isocenter = Some (beam.IsocenterPosition |> mapVVectorToPoint3D)
    MetersetWeight = Some controlPoint.MetersetWeight
    PatientSupportAngle = Some controlPoint.PatientSupportAngle
}

/// Represents one detached beam snapshot extracted from a real ESAPI beam.
let extractBeamSnapshot (beam: Beam) : BeamSnapshotDto =
    let controlPoints = getControlPoints beam
    let firstControlPoint = controlPoints |> List.tryHead
    let lastControlPoint = controlPoints |> List.tryLast

    {
        BeamId = beam.Id
        BeamName = beam.Name |> toOptionalText
        BeamKind = mapBeamKind beam.IsSetupField
        IsTreatmentBeam = not beam.IsSetupField
        IsSetupField = beam.IsSetupField
        GantryDirection = beam.GantryDirection |> mapGantryDirection
        GantryStart = firstControlPoint |> Option.map (fun controlPoint -> controlPoint.GantryAngle)
        GantryStop = lastControlPoint |> Option.map (fun controlPoint -> controlPoint.GantryAngle)
        CouchAngle = firstControlPoint |> Option.map (fun controlPoint -> controlPoint.PatientSupportAngle)
        PatientSupportAngle = firstControlPoint |> Option.map (fun controlPoint -> controlPoint.PatientSupportAngle)
        CollimatorAngle = firstControlPoint |> Option.map (fun controlPoint -> controlPoint.CollimatorAngle)
        Isocenter = Some (beam.IsocenterPosition |> mapVVectorToPoint3D)
        SourcePosition =
            firstControlPoint
            |> Option.bind (fun controlPoint -> tryGetSourcePosition beam controlPoint.GantryAngle)
        ControlPoints = controlPoints |> List.mapi (extractControlPointSnapshot beam)
    }

/// Represents one detached plan snapshot extracted from the current ESAPI plan and treatment-beam list.
let extractPlanSnapshot (patient: Patient) (course: Course) (structureSet: StructureSet) (treatmentBeams: Beam list) (plan: PlanSetup) : PlanSnapshotDto = {
    PatientId = getPatientIdentifier patient
    CourseId = Some course.Id
    StructureSetId = Some structureSet.Id
    PlanId = plan.Id
    PlanName = plan.Name |> toOptionalText
    Beams = treatmentBeams |> List.map extractBeamSnapshot
}

/// Represents one detached BODY-slice list extracted from the current ESAPI BODY structure.
let extractBodyContourSlices (structureSet: StructureSet) (structure: Structure) : Result<BodySliceDto list, string> =
    extractContourSlicesOnImagePlanes structureSet structure
    |> Result.bind (fun slices ->
        let mappedSlices = slices |> List.map (fun (z, contours) -> mapBodySlice z contours)

        if mappedSlices.IsEmpty then
            Error "BODY structure does not contain contour slices for first-version analysis."
        else
            Ok mappedSlices)

/// Represents one detached structure-slice list extracted from the current ESAPI structure.
let extractStructureContourSlices (structureSet: StructureSet) (structure: Structure) : Result<ContourSliceDto list, string> =
    extractContourSlicesOnImagePlanes structureSet structure
    |> Result.map (List.map (fun (z, contours) -> mapContourSlice z contours))

/// Represents one detached BODY snapshot extracted from the current ESAPI BODY structure.
let extractBodySnapshot (structureSet: StructureSet) (body: Structure) : Result<BodySnapshotDto, string> =
    result {
        let! contourSlices = extractBodyContourSlices structureSet body

        let! mesh =
            if isNull body.MeshGeometry then
                Ok None
            else
                body.MeshGeometry
                |> createDetachedMeshSnapshot
                |> Result.bind (fun snapshot -> snapshot |> getDetachedMeshValue |> mapMeshToMeshDto |> Result.map Some)

        let contourBounds = contourSlices |> tryCreateBounds3DFromBodySlices
        let meshBounds = mesh |> Option.bind (fun meshDto -> meshDto.Bounds)

        return {
            StructureId = body.Id
            DisplayName = body.Name |> toOptionalText
            Mesh = mesh
            ContourSlices = contourSlices
            Bounds = contourBounds |> Option.orElse meshBounds
            SliceThicknessMm = getImageSliceThickness structureSet
        }
    }

/// Represents one detached structure snapshot extracted from the current ESAPI structure.
let extractStructureSnapshot (structureSet: StructureSet) (structure: Structure) : Result<StructureSnapshotDto, string> =
    result {
        let! contourSlices = extractStructureContourSlices structureSet structure

        let! mesh =
            if isNull structure.MeshGeometry then
                Ok None
            else
                structure.MeshGeometry
                |> createDetachedMeshSnapshot
                |> Result.bind (fun snapshot -> snapshot |> getDetachedMeshValue |> mapMeshToMeshDto |> Result.map Some)

        let contourBounds = contourSlices |> tryCreateBounds3DFromContourSlices
        let meshBounds = mesh |> Option.bind (fun meshDto -> meshDto.Bounds)

        return {
            StructureId = structure.Id
            DisplayName = structure.Name |> toOptionalText
            Mesh = mesh
            ContourSlices = contourSlices
            Bounds = contourBounds |> Option.orElse meshBounds
        }
    }

/// Represents one detached accessory DTO extracted from a real ESAPI structure.
let extractAccessoryModel (kind: AccessoryKindDto) (structureSet: StructureSet) (structure: Structure) : Result<AccessoryModelDto, string> =
    extractStructureSnapshot structureSet structure
    |> Result.map (fun snapshot -> {
        AccessoryId = snapshot.StructureId
        Kind = kind
        DisplayName = snapshot.DisplayName |> Option.defaultValue snapshot.StructureId
        Mesh = snapshot.Mesh
        Structure = Some snapshot
        Bounds = snapshot.Bounds
        Offset = None
        IsEnabled = true
    })
