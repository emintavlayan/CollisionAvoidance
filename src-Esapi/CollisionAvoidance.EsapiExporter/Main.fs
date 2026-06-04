module CollisionAvoidance.EsapiExporter.Main

open System.Windows.Forms
open Shared
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.ExportWorkflow
open CollisionAvoidance.EsapiExporter.SafeServerClient

/// Creates a compile-safe placeholder export context until real ESAPI adapters are wired in.
let createPlaceholderContext () : ExportContext =
    {
        PatientId = Some "placeholder-patient"
        CourseId = Some "placeholder-course"
        PlanContext =
            Some {
                PatientId = "placeholder-patient"
                CourseId = Some "placeholder-course"
                StructureSetId = Some "placeholder-structure-set"
                PlanId = "placeholder-plan"
                PlanName = Some "Placeholder plan"
                Beams = []
            }
        StructureSetContext =
            Some {
                StructureSetId = Some "placeholder-structure-set"
                Structures =
                    [
                        {
                            StructureId = "BODY"
                            DisplayName = Some "External"
                            Mesh = None
                            ContourSlices = []
                            SliceThicknessMm = None
                        }
                    ]
            }
        SamplingSettings = Some defaultSamplingSettings
        OutputDirectory = None
    }

/// Starts the ESAPI exporter entry point and shows a simple success or failure message.
[<EntryPoint>]
let main _argv =
    let result =
        createPlaceholderContext ()
        |> exportCollisionRun defaultServerBaseUrl
        |> Async.RunSynchronously

    match result with
    | Ok outcome ->
        MessageBox.Show($"Collision request exported to {outcome.JsonFilePath}.", "CollisionAvoidance ESAPI exporter")
        |> ignore
        0
    | Error message ->
        MessageBox.Show(message, "CollisionAvoidance ESAPI exporter")
        |> ignore
        1
