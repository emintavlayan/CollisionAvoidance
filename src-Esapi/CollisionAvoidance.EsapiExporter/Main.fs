namespace CollisionAvoidance.EsapiExporter

open System.Runtime.CompilerServices
open System.Windows.Forms
open VMS.TPS.Common.Model.API
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.ExportWorkflow
open CollisionAvoidance.EsapiExporter.SafeServerClient

[<assembly: ESAPIScript(IsWriteable = false)>]
do ()

/// Represents the ESAPI script entry point that exports detached collision DTOs from Eclipse.
[<CompilerGenerated>]
type Script() =

    /// Executes the ESAPI exporter workflow for the current Eclipse script context.
    member _.Execute(context: ScriptContext) =
        let exportContext = {
            ScriptContext = context
            SamplingSettings = Some defaultSamplingSettings
            OutputDirectory = None
        }

        let result =
            exportCollisionRun defaultServerBaseUrl exportContext
            |> Async.RunSynchronously

        match result with
        | Ok outcome ->
            let message =
                match outcome.SubmittedRun with
                | Some submission when submission.RunPageUrl.IsSome ->
                    $"Collision request exported to {outcome.JsonFilePath}.{System.Environment.NewLine}{System.Environment.NewLine}SAFE run page: {submission.RunPageUrl.Value}"
                | _ ->
                    $"Collision request exported to {outcome.JsonFilePath}."

            MessageBox.Show(message, "CollisionAvoidance ESAPI exporter")
            |> ignore
        | Error error ->
            MessageBox.Show(error, "CollisionAvoidance ESAPI exporter")
            |> ignore
