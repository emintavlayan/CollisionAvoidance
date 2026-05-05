namespace VMS.TPS

open VMS.TPS.Common.Model.API
open VMS.TPS.Workflow
open VMS.TPS.DebugHelpers

[<assembly : ESAPIScript(IsWriteable = true)>]
do ()

[<System.Runtime.CompilerServices.CompilerGeneratedAttribute>]
type Script() =

    /// Executes the ESAPI script
    member __.Execute(context : ScriptContext) =

        let result =
            runCollisionCheckWorkflow context

        match result with
        | Ok message ->
            showMessageBox message

        | Error error ->
            showMessageBox error