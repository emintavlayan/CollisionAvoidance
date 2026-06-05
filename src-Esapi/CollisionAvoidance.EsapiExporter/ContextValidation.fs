module CollisionAvoidance.EsapiExporter.ContextValidation

open System
open Shared
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction

/// Represents the detached ESAPI export inputs gathered before validation.
type ExportContext = {
    PatientId: string option
    CourseId: string option
    PlanContext: EsapiPlanLike option
    StructureSetContext: EsapiStructureSetLike option
    SamplingSettings: SamplingSettingsDto option
    OutputDirectory: string option
}

/// Represents the validated ESAPI export inputs that are safe to pass into extraction.
type ValidatedEsapiContext = {
    Patient: string
    Course: string
    Plan: EsapiPlanLike
    StructureSet: EsapiStructureSetLike
    Body: EsapiStructureLike
    CouchSurface: EsapiStructureLike option
    TreatmentBeams: EsapiBeamLike list
    SamplingSettings: SamplingSettingsDto
}

/// Returns the first practical detached sampling settings used by the exporter.
let defaultSamplingSettings =
    {
        BodySampleStepMm = Some (Length.millimeters 2.5)
        BeamSampleStepMm = Some (Length.millimeters 5.0)
        BeamAxisOffsetMm = Some (Length.millimeters 550.0)
        ClearanceRadiusMm = Some (Length.millimeters 390.0)
        ClearanceDistanceMm = Some (Length.millimeters 390.0)
        CollisionToleranceMm = Some (Length.millimeters 1.0)
        ArcStepDegrees = Some 1.0
    }

/// Validates that an optional text input is present and non-blank.
let validateRequiredText errorMessage (value: string option) =
    match value with
    | Some text when String.IsNullOrWhiteSpace text |> not -> Ok text
    | _ -> Error errorMessage

/// Normalizes a structure identifier for case-insensitive matching.
let normalizeStructureIdentifier (value: string) =
    value.Trim().ToUpperInvariant()

/// Checks whether a detached structure projection should be treated as BODY.
let isBodyStructure (structureContext: EsapiStructureLike) =
    normalizeStructureIdentifier structureContext.StructureId = "BODY"

/// Checks whether a detached structure projection looks like a couch surface candidate.
let isCouchSurfaceStructure (structureContext: EsapiStructureLike) =
    let identifiers =
        [
            structureContext.StructureId
            structureContext.DisplayName |> Option.defaultValue String.Empty
        ]
        |> List.map normalizeStructureIdentifier

    identifiers
    |> List.exists (fun identifier -> identifier.Contains "COUCH")

/// Validates that a patient value is present and returns it.
let validatePatient (ctx: ExportContext) : Result<string, string> =
    validateRequiredText "No patient is currently loaded." ctx.PatientId

/// Validates that a course value is present and returns it.
let validateCourse (ctx: ExportContext) : Result<string, string> =
    validateRequiredText "No course is currently loaded." ctx.CourseId

/// Validates that a plan value is present and returns it.
let validatePlan (ctx: ExportContext) : Result<EsapiPlanLike, string> =
    match ctx.PlanContext with
    | Some planContext -> Ok planContext
    | None -> Error "No plan is currently loaded."

/// Validates that a structure-set value is present and returns it.
let validateStructureSet (ctx: ExportContext) : Result<EsapiStructureSetLike, string> =
    match ctx.StructureSetContext with
    | Some structureSetContext -> Ok structureSetContext
    | None -> Error "No structure set is currently loaded."

/// Validates that a BODY structure value is present and returns it.
let validateBody (_ctx: ExportContext) (structureSet: EsapiStructureSetLike) : Result<EsapiStructureLike, string> =
    structureSet.Structures
    |> List.tryFind isBodyStructure
    |> function
        | Some bodyStructure -> Ok bodyStructure
        | None -> Error "BODY structure was not found."

/// Tries to find an optional couch-surface structure without failing validation.
let tryFindOptionalCouchSurface (_ctx: ExportContext) (structureSet: EsapiStructureSetLike) =
    structureSet.Structures
    |> List.tryFind (fun structureContext -> isBodyStructure structureContext |> not && isCouchSurfaceStructure structureContext)

/// Validates that at least one treatment beam is present and returns the list.
let validateTreatmentBeams (_ctx: ExportContext) (plan: EsapiPlanLike) : Result<EsapiBeamLike list, string> =
    let treatmentBeams = plan.Beams |> List.filter (fun beamContext -> not beamContext.IsSetupField)

    match treatmentBeams with
    | [] -> Error "No treatment beams were supplied."
    | beams -> Ok beams

/// Resolves explicit or default sampling settings for the export request.
let resolveSamplingSettings (ctx: ExportContext) =
    ctx.SamplingSettings |> Option.defaultValue defaultSamplingSettings

/// Validates the full ESAPI export context and collects all missing required inputs in one pass.
let validateContext (ctx: ExportContext) : Result<ValidatedEsapiContext, string list> =
    let errors = ResizeArray<string>()

    let patient =
        match validatePatient ctx with
        | Ok patientId -> Some patientId
        | Error error ->
            errors.Add error
            None

    let course =
        match validateCourse ctx with
        | Ok courseId -> Some courseId
        | Error error ->
            errors.Add error
            None

    let plan =
        match validatePlan ctx with
        | Ok planContext -> Some planContext
        | Error error ->
            errors.Add error
            None

    let structureSet =
        match validateStructureSet ctx with
        | Ok structureSetContext -> Some structureSetContext
        | Error error ->
            errors.Add error
            None

    let body =
        match structureSet with
        | Some structureSetContext ->
            match validateBody ctx structureSetContext with
            | Ok bodyContext -> Some bodyContext
            | Error error ->
                errors.Add error
                None
        | None -> None

    let treatmentBeams =
        match plan with
        | Some planContext ->
            match validateTreatmentBeams ctx planContext with
            | Ok beamContexts -> Some beamContexts
            | Error error ->
                errors.Add error
                None
        | None -> None

    if errors.Count > 0 then
        Error (List.ofSeq errors)
    else
        Ok {
            Patient = patient.Value
            Course = course.Value
            Plan = plan.Value
            StructureSet = structureSet.Value
            Body = body.Value
            CouchSurface = structureSet |> Option.bind (tryFindOptionalCouchSurface ctx)
            TreatmentBeams = treatmentBeams.Value
            SamplingSettings = resolveSamplingSettings ctx
        }
