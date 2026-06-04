module CollisionAvoidance.EsapiExporter.ExportWorkflow

open System
open Shared
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.PatientIdObfuscation
open CollisionAvoidance.EsapiExporter.SafeServerClient

type ExportContext = {
    PatientContext: obj option
    CourseContext: obj option
    PlanContext: obj option
    StructureSetContext: obj option
    BodyStructureContext: obj option
    TreatmentBeamContexts: obj list
}

/// Validates context, extracts detached DTOs, obfuscates the patient id, posts to SAFE, and opens the run page.
let exportCollisionRun (serverBaseUrl: Uri) (context: ExportContext) : Async<Uri> =
    match validatePatient context.PatientContext with
    | Ok () -> ()
    | Error message -> invalidOp message

    match validateCourse context.CourseContext with
    | Ok () -> ()
    | Error message -> invalidOp message

    match validatePlan context.PlanContext with
    | Ok () -> ()
    | Error message -> invalidOp message

    match validateStructureSet context.StructureSetContext with
    | Ok () -> ()
    | Error message -> invalidOp message

    match validateBody context.BodyStructureContext with
    | Ok () -> ()
    | Error message -> invalidOp message

    match validateTreatmentBeams context.TreatmentBeamContexts with
    | Ok () -> ()
    | Error message -> invalidOp message

    let planContext = context.PlanContext |> Option.defaultWith (fun () -> invalidOp "Plan context is required.")
    let structureSetContext =
        context.StructureSetContext
        |> Option.defaultWith (fun () -> invalidOp "Structure set context is required.")
    let bodyStructureContext =
        context.BodyStructureContext
        |> Option.defaultWith (fun () -> invalidOp "BODY structure context is required.")

    let request = extractCollisionRunRequest planContext structureSetContext bodyStructureContext
    let obfuscatedRequest = {
        request with
            Plan = {
                request.Plan with
                    PatientId = obfuscatePatientId request.Plan.PatientId
            }
    }

    async {
        let! runPageUrl = postCollisionRunRequest serverBaseUrl obfuscatedRequest
        openCollisionRunPage runPageUrl
        return runPageUrl
    }
