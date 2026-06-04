module CollisionAvoidance.EsapiExporter.ContextValidation

/// Validates that a patient is available in the current ESAPI context.
let validatePatient (patientContext: obj option) =
    match patientContext with
    | Some _ -> Ok ()
    | None -> Error "Patient context was not supplied."

/// Validates that a course is available in the current ESAPI context.
let validateCourse (courseContext: obj option) =
    match courseContext with
    | Some _ -> Ok ()
    | None -> Error "Course context was not supplied."

/// Validates that a plan is available in the current ESAPI context.
let validatePlan (planContext: obj option) =
    match planContext with
    | Some _ -> Ok ()
    | None -> Error "Plan context was not supplied."

/// Validates that a structure set is available in the current ESAPI context.
let validateStructureSet (structureSetContext: obj option) =
    match structureSetContext with
    | Some _ -> Ok ()
    | None -> Error "Structure set context was not supplied."

/// Validates that a BODY structure can be resolved for export.
let validateBody (bodyStructureContext: obj option) =
    match bodyStructureContext with
    | Some _ -> Ok ()
    | None -> Error "BODY structure context was not supplied."

/// Validates that treatment beams are available for extraction.
let validateTreatmentBeams (treatmentBeamContexts: obj list) =
    match treatmentBeamContexts with
    | _ :: _ -> Ok ()
    | [] -> Error "No treatment beams were supplied."
