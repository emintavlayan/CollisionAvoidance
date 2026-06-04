module CollisionAvoidance.EsapiExporter.ExportWorkflow

open System
open System.IO
open FsToolkit.ErrorHandling
open Shared
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.PatientIdObfuscation
open CollisionAvoidance.EsapiExporter.SafeServerClient

type ExportContext = {
    PatientId: string option
    CourseId: string option
    PlanContext: EsapiPlanLike option
    StructureSetId: string option
    BodyContext: EsapiBodyLike option
    TreatmentBeamContexts: EsapiBeamLike list
    SamplingSettings: SamplingSettingsDto
    Accessories: AccessoryModelDto list
    OutputDirectory: string option
}

type ExportOutcome = {
    JsonFilePath: string
    SubmittedRun: CollisionRunSubmissionResult option
}

/// Creates an ESAPI-like extraction payload from the validated export context.
let createRunContext (context: ExportContext) (planContext: EsapiPlanLike) (bodyContext: EsapiBodyLike) : EsapiCollisionRunLike =
    let updatedPlanContext = {
        planContext with
            PatientId = context.PatientId |> Option.defaultValue planContext.PatientId
            CourseId = context.CourseId |> Option.orElse planContext.CourseId
            StructureSetId = context.StructureSetId |> Option.orElse planContext.StructureSetId
            Beams = context.TreatmentBeamContexts
    }

    {
        Plan = updatedPlanContext
        Body = bodyContext
        SamplingSettings = context.SamplingSettings
        Accessories = context.Accessories
    }

/// Obfuscates the patient id on a detached collision run request before serialization or submission.
let obfuscateRequestPatientId (request: CollisionRunRequestDto) = {
    request with
        Plan = {
            request.Plan with
                PatientId = obfuscatePatientId request.Plan.PatientId
        }
}

/// Chooses an output directory for local JSON fallback files.
let resolveOutputDirectory (context: ExportContext) =
    context.OutputDirectory
    |> Option.defaultValue (Path.Combine(Path.GetTempPath(), "CollisionAvoidance"))

/// Validates context, writes a local JSON fallback, then attempts SAFE submission and page launch.
let exportCollisionRun (serverBaseUrl: Uri) (context: ExportContext) : Async<Result<ExportOutcome, string>> =
    async {
        let exportResult =
            result {
                let! patientId = validatePatient context.PatientId
                let! _courseId = validateCourse context.CourseId
                let! planContext = validatePlan context.PlanContext
                let! _structureSetId = validateStructureSet context.StructureSetId
                let! bodyContext = validateBody context.BodyContext
                let! treatmentBeams = validateTreatmentBeams context.TreatmentBeamContexts

                let request =
                    createRunContext { context with PatientId = Some patientId; TreatmentBeamContexts = treatmentBeams } planContext bodyContext
                    |> extractCollisionRunRequest
                    |> Result.map obfuscateRequestPatientId

                let! detachedRequest = request
                let outputDirectory = resolveOutputDirectory context
                let! jsonFilePath = writeCollisionRunRequestToJsonFile outputDirectory detachedRequest
                let! submissionAttempt = postCollisionRunRequest serverBaseUrl detachedRequest |> Async.RunSynchronously |> Ok

                match submissionAttempt with
                | Ok submission ->
                    match submission.RunPageUrl with
                    | Some runPageUrl ->
                        do! openCollisionRunPage runPageUrl
                        return { JsonFilePath = jsonFilePath; SubmittedRun = Some submission }
                    | None ->
                        return { JsonFilePath = jsonFilePath; SubmittedRun = Some submission }
                | Error _ ->
                    return { JsonFilePath = jsonFilePath; SubmittedRun = None }
            }

        return exportResult
    }
