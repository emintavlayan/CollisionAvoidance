(**
    Module: ContextRetrievalSafe
    Purpose: 
        - Provides safe helper functions for retrieving entities from the VMS TPS scripting context,
        - including patient, plan, course, and structure set retrieval with error handling.
    Why:
        - Centralizes context retrieval logic to avoid duplication,
        - ensures consistent error handling when accessing context entities.

    Notes:
        - All functions here are designed to be side-effect-safe where possible.
        - Depends on VMS.TPS.Common.Model.API for core types.
*)

module VMS.TPS.ContextRetrievalSafe

open VMS.TPS.Common.Model.API

/// Gets the currently loaded patient or returns an error
let tryGetCurrentPatient (context : ScriptContext) =
    if isNull context.Patient then
        Error "No patient is currently loaded."
    else
        Ok context.Patient

/// Gets the currently loaded course or returns an error
let tryGetCurrentCourse (context : ScriptContext) =
    if isNull context.Course then
        Error "No course is currently loaded."
    else
        Ok context.Course

/// Gets the currently loaded plan or returns an error
let tryGetCurrentPlan (context : ScriptContext) =
    if isNull context.PlanSetup then
        Error "No plan is currently loaded."
    else
        Ok context.Patient

/// Gets the currently loaded structure set or returns an error
let tryGetCurrentStructureSet (context : ScriptContext) =
    if isNull context.StructureSet then
        Error "No structure set is currently loaded."
    else
        Ok context.StructureSet
