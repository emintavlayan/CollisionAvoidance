module CollisionAvoidance.EsapiExporter.ExportWorkflow

open System
open System.IO
open FsToolkit.ErrorHandling
open Shared
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.PatientIdObfuscation
open CollisionAvoidance.EsapiExporter.SafeServerClient

type ExportOutcome = {
    JsonFilePath: string
    SubmittedRun: CollisionRunSubmissionResult option
}

/// Creates an ESAPI-like extraction payload from the validated export context.
let createRunContext (context: ValidatedEsapiContext) : EsapiCollisionRunLike =
    let updatedPlanContext = {
        context.Plan with
            PatientId = context.Patient
            CourseId = Some context.Course
            StructureSetId = context.StructureSet.StructureSetId |> Option.orElse context.Plan.StructureSetId
            Beams = context.TreatmentBeams
    }

    {
        Plan = updatedPlanContext
        Body = context.Body
        SamplingSettings = context.SamplingSettings
        Accessories = []
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
                let! validatedContext =
                    validateContext context
                    |> Result.mapError (String.concat Environment.NewLine)

                let request = createRunContext validatedContext |> extractCollisionRunRequest |> Result.map obfuscateRequestPatientId
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
