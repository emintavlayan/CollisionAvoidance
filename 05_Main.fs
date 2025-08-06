namespace VMS.TPS

open VMS.TPS.Common.Model.API
open Workflow
open FsToolkit.ErrorHandling
open System.Reflection

[<assembly : ESAPIScript(IsWriteable = true)>]
do ()

[<System.Runtime.CompilerServices.CompilerGeneratedAttribute>]
type Script() =
    member __.Execute(context : ScriptContext) =

        let result = result {
            // Gets the currently loaded patient and begins modifications
            let! patient = Utilities.tryGetCurrentPlan context



            return 0
        }

        match result with
        | Ok messages ->
            // Show all success and error messages after workflow completes
            Utilities.showMessageBox (String.concat "\n" messages)
        | Error msg ->
            // Show fatal setup error (context, course, or plan loading)
            Utilities.showMessageBox $"[Setup Error] {msg}"
