namespace VMS.TPS

open VMS.TPS.Common.Model.API
open Workflow
open FsToolkit.ErrorHandling

[<assembly : ESAPIScript(IsWriteable = true)>]
do ()

[<System.Runtime.CompilerServices.CompilerGeneratedAttribute>]
type Script() =
    member __.Execute(context : ScriptContext) =

        let result = result {
            // not implemented yet
            return 0
        }

        match result with
        | Ok messages ->
            // Show all success and error messages after workflow completes
            DebugHelpers.showMessageBox "hi"
        | Error msg ->
            // Show fatal setup error (context, course, or plan loading)
            DebugHelpers.showMessageBox $"shit happens"
