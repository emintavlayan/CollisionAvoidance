(**
    Module: Utilities
    Purpose: 
        - Provides helper functions for interacting with the VMS TPS scripting API,
        - including debugging utilities and patient/plan retrieval helpers.
    Why:
        - Centralizing these utilities avoids duplication and
        - makes scripts more readable,
        - ensuring consistent error handling and debug output.

    Notes:
        - All functions here are designed to be side-effect-safe where possible.
        - Depends on VMS.TPS.Common.Model.API for core types.
*)

module VMS.TPS.Utilities

open VMS.TPS.Common.Model.API


/// Shows the given message for debugging purposes
let showMessageBox (message : string) =
    System.Windows.Forms.MessageBox.Show(message)
    |> ignore

/// Gets the currently loaded patient or returns an error
let tryGetCurrentPlan (context : ScriptContext) =
    if isNull context.PlanSetup then
        Error "No plan is currently loaded."
    else
        Ok context.Patient
