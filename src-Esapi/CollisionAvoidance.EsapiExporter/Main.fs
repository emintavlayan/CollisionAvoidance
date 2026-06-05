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
                Beams =
                    [
                        {
                            BeamId = "placeholder-beam"
                            BeamName = Some "Placeholder beam"
                            IsSetupField = false
                            GantryDirection = NotSpecified
                            GantryStart = Some 0.0
                            GantryStop = Some 0.0
                            CouchAngle = Some 0.0
                            PatientSupportAngle = Some 0.0
                            CollimatorAngle = Some 0.0
                            Isocenter = Some { X = 0.0; Y = 0.0; Z = 0.0 }
                            SourcePosition = Some { X = 0.0; Y = 1000.0; Z = 0.0 }
                            ControlPoints =
                                [
                                    {
                                        Index = 0
                                        GantryAngle = 0.0
                                        CouchAngle = Some 0.0
                                        PatientSupportAngle = Some 0.0
                                        CollimatorAngle = Some 0.0
                                        SourcePosition = Some { X = 0.0; Y = 1000.0; Z = 0.0 }
                                        Isocenter = Some { X = 0.0; Y = 0.0; Z = 0.0 }
                                        MetersetWeight = Some 0.0
                                    }
                                ]
                        }
                    ]
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
                            ContourSlices =
                                [
                                    {
                                        Z = 0.0
                                        Contours =
                                            [
                                                [
                                                    { X = -50.0; Y = -50.0; Z = 0.0 }
                                                    { X = 50.0; Y = -50.0; Z = 0.0 }
                                                    { X = 50.0; Y = 50.0; Z = 0.0 }
                                                    { X = -50.0; Y = 50.0; Z = 0.0 }
                                                ]
                                            ]
                                    }
                                ]
                            SliceThicknessMm = Some (Length.millimeters 5.0)
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
