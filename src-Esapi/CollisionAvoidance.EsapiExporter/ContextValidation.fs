module CollisionAvoidance.EsapiExporter.ContextValidation

open System
open Shared
open VMS.TPS.Common.Model.API

/// Represents the ESAPI export inputs gathered before validation starts.
type ExportContext = {
    ScriptContext: ScriptContext
    SamplingSettings: SamplingSettingsDto option
    OutputDirectory: string option
}

/// Represents the validated ESAPI entities that are safe to pass into extraction.
type ValidatedEsapiContext = {
    ScriptContext: ScriptContext
    Patient: Patient
    Course: Course
    Plan: PlanSetup
    StructureSet: StructureSet
    Body: Structure
    CouchSurface: Structure option
    TreatmentBeams: Beam list
    SamplingSettings: SamplingSettingsDto
}

/// Represents the first practical detached sampling settings used by the exporter.
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

/// Represents a normalized structure identifier for case-insensitive matching.
let normalizeStructureIdentifier (value: string) =
    value.Trim().ToUpperInvariant()

/// Represents whether a structure identifier matches BODY.
let isBodyStructureId (structureId: string) =
    normalizeStructureIdentifier structureId = "BODY"

/// Represents whether a pair of structure identifiers looks like a couch surface.
let looksLikeCouchSurfaceIdentifier (structureId: string) (displayName: string option) =
    [ structureId; displayName |> Option.defaultValue String.Empty ]
    |> List.map normalizeStructureIdentifier
    |> List.exists (fun identifier -> identifier = "COUCHSURFACE" || identifier.Contains "COUCH")

/// Represents the currently loaded patient when it exists.
let validatePatient (ctx: ExportContext) : Result<Patient, string> =
    if isNull ctx.ScriptContext.Patient then
        Error "No patient is currently loaded."
    else
        Ok ctx.ScriptContext.Patient

/// Represents the currently loaded course when it exists.
let validateCourse (ctx: ExportContext) : Result<Course, string> =
    if isNull ctx.ScriptContext.Course then
        Error "No course is currently loaded."
    else
        Ok ctx.ScriptContext.Course

/// Represents the currently loaded plan when it exists.
let validatePlan (ctx: ExportContext) : Result<PlanSetup, string> =
    if isNull ctx.ScriptContext.PlanSetup then
        Error "No plan is currently loaded."
    else
        Ok ctx.ScriptContext.PlanSetup

/// Represents the currently loaded structure set when it exists.
let validateStructureSet (ctx: ExportContext) : Result<StructureSet, string> =
    if isNull ctx.ScriptContext.StructureSet then
        Error "No structure set is currently loaded."
    else
        Ok ctx.ScriptContext.StructureSet

/// Represents the BODY structure when it can be found in the current structure set.
let validateBody (_ctx: ExportContext) (structureSet: StructureSet) : Result<Structure, string> =
    structureSet.Structures
    |> Seq.tryFind (fun structure -> isBodyStructureId structure.Id)
    |> function
        | Some body -> Ok body
        | None -> Error "BODY structure was not found."

/// Represents the optional couch-surface structure when it can be found in the current structure set.
let tryFindOptionalCouchSurface (_ctx: ExportContext) (structureSet: StructureSet) =
    structureSet.Structures
    |> Seq.tryFind (fun structure ->
        isBodyStructureId structure.Id |> not
        && looksLikeCouchSurfaceIdentifier structure.Id (Some structure.Name))

/// Represents the treatment beams after setup fields have been filtered out.
let validateTreatmentBeams (_ctx: ExportContext) (plan: PlanSetup) : Result<Beam list, string> =
    let treatmentBeams =
        plan.Beams
        |> Seq.filter (fun beam -> not beam.IsSetupField)
        |> Seq.toList

    match treatmentBeams with
    | [] -> Error "No treatment beams were supplied."
    | beams -> Ok beams

/// Represents the resolved sampling settings for one export request.
let resolveSamplingSettings (ctx: ExportContext) =
    ctx.SamplingSettings |> Option.defaultValue defaultSamplingSettings

/// Represents the full validated ESAPI export context after all missing-entity checks succeed.
let validateContext (ctx: ExportContext) : Result<ValidatedEsapiContext, string list> =
    let errors = ResizeArray<string>()

    let patient =
        match validatePatient ctx with
        | Ok value -> Some value
        | Error error ->
            errors.Add error
            None

    let course =
        match validateCourse ctx with
        | Ok value -> Some value
        | Error error ->
            errors.Add error
            None

    let plan =
        match validatePlan ctx with
        | Ok value -> Some value
        | Error error ->
            errors.Add error
            None

    let structureSet =
        match validateStructureSet ctx with
        | Ok value -> Some value
        | Error error ->
            errors.Add error
            None

    let body =
        match structureSet with
        | Some value ->
            match validateBody ctx value with
            | Ok structure -> Some structure
            | Error error ->
                errors.Add error
                None
        | None -> None

    let treatmentBeams =
        match plan with
        | Some value ->
            match validateTreatmentBeams ctx value with
            | Ok beams -> Some beams
            | Error error ->
                errors.Add error
                None
        | None -> None

    if errors.Count > 0 then
        Error (errors |> Seq.toList)
    else
        Ok {
            ScriptContext = ctx.ScriptContext
            Patient = patient.Value
            Course = course.Value
            Plan = plan.Value
            StructureSet = structureSet.Value
            Body = body.Value
            CouchSurface = structureSet |> Option.bind (tryFindOptionalCouchSurface ctx)
            TreatmentBeams = treatmentBeams.Value
            SamplingSettings = resolveSamplingSettings ctx
        }
