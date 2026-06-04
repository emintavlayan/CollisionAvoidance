module CollisionAvoidance.EsapiExporter.Main

open System.Windows.Forms
open Shared
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.ExportWorkflow
open CollisionAvoidance.EsapiExporter.SafeServerClient

/// Creates a compile-safe placeholder export context until real ESAPI adapters are wired in.
let createPlaceholderContext () =
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
        StructureSetId = Some "placeholder-structure-set"
        BodyContext =
            Some {
                StructureId = "BODY"
                DisplayName = Some "External"
                Mesh = None
                ContourSlices = []
                SliceThicknessMm = None
            }
        TreatmentBeamContexts = []
        SamplingSettings =
            {
                BodySampleStepMm = Some 2.5
                BeamSampleStepMm = Some 5.0
                BeamAxisOffsetMm = Some 550.0
                ClearanceRadiusMm = Some 390.0
                ClearanceDistanceMm = Some 390.0
                CollisionToleranceMm = Some 1.0
                ArcStepDegrees = Some 1.0
            }
        Accessories = []
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
