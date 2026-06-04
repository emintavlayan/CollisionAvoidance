module CollisionAvoidance.EsapiExporter.ContextValidation

open FsToolkit.ErrorHandling

/// Validates that a patient value is present and returns it.
let validatePatient (patientContext: 'patient option) : Result<'patient, string> =
    patientContext |> Result.requireSome "No patient is currently loaded."

/// Validates that a course value is present and returns it.
let validateCourse (courseContext: 'course option) : Result<'course, string> =
    courseContext |> Result.requireSome "No course is currently loaded."

/// Validates that a plan value is present and returns it.
let validatePlan (planContext: 'plan option) : Result<'plan, string> =
    planContext |> Result.requireSome "No plan is currently loaded."

/// Validates that a structure-set value is present and returns it.
let validateStructureSet (structureSetContext: 'structureSet option) : Result<'structureSet, string> =
    structureSetContext |> Result.requireSome "No structure set is currently loaded."

/// Validates that a BODY structure value is present and returns it.
let validateBody (bodyStructureContext: 'body option) : Result<'body, string> =
    bodyStructureContext |> Result.requireSome "BODY structure was not found."

/// Validates that at least one treatment beam is present and returns the list.
let validateTreatmentBeams (treatmentBeamContexts: 'beam list) : Result<'beam list, string> =
    match treatmentBeamContexts with
    | [] -> Error "No treatment beams were supplied."
    | beams -> Ok beams
