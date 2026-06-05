module EsapiExporter.Tests

open CollisionAvoidance.EsapiExporter.PatientIdObfuscation
open CollisionAvoidance.EsapiExporter.EsapiGeometryMapping
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.ExportWorkflow
open Shared
open Xunit

let bodyContour =
    {
        Z = 0.0
        Contours =
            [
                [
                    { X = 0.0; Y = 0.0; Z = 0.0 }
                    { X = 10.0; Y = 0.0; Z = 0.0 }
                    { X = 10.0; Y = 10.0; Z = 0.0 }
                    { X = 0.0; Y = 10.0; Z = 0.0 }
                ]
            ]
    }

let samplePlan : EsapiPlanLike =
    {
        PatientId = "PATIENT-001"
        CourseId = Some "COURSE-1"
        StructureSetId = Some "SS-1"
        PlanId = "PLAN-1"
        PlanName = Some "Plan 1"
        Beams =
            [
                {
                    BeamId = "Beam-1"
                    BeamName = Some "Beam 1"
                    IsSetupField = false
                    GantryDirection = NotSpecified
                    GantryStart = Some 0.0
                    GantryStop = Some 0.0
                    CouchAngle = Some 0.0
                    PatientSupportAngle = Some 0.0
                    CollimatorAngle = Some 0.0
                    Isocenter = None
                    SourcePosition = None
                    ControlPoints = []
                }
            ]
    }

let planWithSetupAndTreatmentBeams : EsapiPlanLike =
    {
        samplePlan with
            Beams =
                [
                    {
                        samplePlan.Beams.Head with
                            BeamId = "Setup-1"
                            IsSetupField = true
                    }
                    samplePlan.Beams.Head
                ]
    }

let planWithSetupBeamsOnly : EsapiPlanLike =
    {
        samplePlan with
            Beams =
                [
                    {
                        samplePlan.Beams.Head with
                            BeamId = "Setup-1"
                            IsSetupField = true
                    }
                ]
    }

let bodyStructure : EsapiStructureLike =
    {
        StructureId = "BODY"
        DisplayName = Some "External"
        Mesh = None
        ContourSlices = [ bodyContour ]
        SliceThicknessMm = Some (Length.millimeters 2.5)
    }

let meshOnlyBodyStructure : EsapiStructureLike =
    {
        StructureId = "BODY"
        DisplayName = Some "External"
        Mesh =
            Some {
                Vertices =
                    [
                        { X = 0.0; Y = 0.0; Z = 0.0 }
                        { X = 1.0; Y = 0.0; Z = 0.0 }
                        { X = 0.0; Y = 1.0; Z = 0.0 }
                    ]
                TriangleIndices = [ 0; 1; 2 ]
                Bounds = Some { X = 0.0; Y = 0.0; Z = 0.0; SizeX = 1.0; SizeY = 1.0; SizeZ = 1.0 }
                CanFreeze = true
            }
        ContourSlices = []
        SliceThicknessMm = Some (Length.millimeters 2.5)
    }

let couchStructure : EsapiStructureLike =
    {
        StructureId = "CouchSurface"
        DisplayName = Some "Couch Surface"
        Mesh = None
        ContourSlices = []
        SliceThicknessMm = Some (Length.millimeters 2.5)
    }

let createContext (structures: EsapiStructureLike list) : ExportContext =
    {
        PatientId = Some "PATIENT-001"
        CourseId = Some "COURSE-1"
        PlanContext = Some samplePlan
        StructureSetContext = Some { StructureSetId = Some "SS-1"; Structures = structures }
        SamplingSettings = None
        OutputDirectory = None
    }

let createContextWithPlan (plan: EsapiPlanLike) (structures: EsapiStructureLike list) : ExportContext =
    { createContext structures with PlanContext = Some plan }

[<Fact>]
let ``Patient id obfuscation is deterministic for the same raw id`` () =
    let rawId = "PATIENT-001"

    let firstHash = obfuscatePatientId rawId
    let secondHash = obfuscatePatientId rawId

    Assert.Equal(firstHash, secondHash)
    Assert.NotEqual<string>(rawId, firstHash)

[<Fact>]
let ``Mapping a vector-like value creates a detached point`` () =
    let point = mapVVectorToPoint3D { X = 1.0; Y = 2.0; Z = 3.0 }

    Assert.Equal(1.0, point.X)
    Assert.Equal(2.0, point.Y)
    Assert.Equal(3.0, point.Z)

[<Fact>]
let ``Mapping mesh bounds creates the expected detached bounds`` () =
    let bounds =
        mapMeshBoundsToBounds3D {
            X = -1.0
            Y = -2.0
            Z = -3.0
            SizeX = 5.0
            SizeY = 6.0
            SizeZ = 7.0
        }

    Assert.Equal(-1.0, bounds.Min.X)
    Assert.Equal(4.0, bounds.Max.X)
    Assert.Equal(4.0, bounds.Max.Z)

[<Fact>]
let ``Context validation collects multiple required context errors`` () =
    let context =
        {
            PatientId = None
            CourseId = None
            PlanContext = None
            StructureSetContext = None
            SamplingSettings = None
            OutputDirectory = None
        }

    let result = validateContext context

    match result with
    | Ok _ -> failwith "Expected context validation to fail."
    | Error errors ->
        Assert.Contains("No patient is currently loaded.", errors)
        Assert.Contains("No course is currently loaded.", errors)
        Assert.Contains("No plan is currently loaded.", errors)
        Assert.Contains("No structure set is currently loaded.", errors)

[<Fact>]
let ``Context validation requires a BODY structure`` () =
    let context = createContext [ couchStructure ]

    let result = validateContext context

    match result with
    | Ok _ -> failwith "Expected context validation to fail when BODY is missing."
    | Error errors ->
        Assert.Contains("BODY structure was not found.", errors)

[<Fact>]
let ``Context validation allows a missing couch surface`` () =
    let context = createContext [ bodyStructure ]

    let result = validateContext context

    match result with
    | Ok validatedContext ->
        Assert.Equal("BODY", validatedContext.Body.StructureId)
        Assert.True(validatedContext.CouchSurface.IsNone)
    | Error errors ->
        failwith (String.concat "; " errors)

[<Fact>]
let ``Context validation returns an optional couch surface when present`` () =
    let context = createContext [ bodyStructure; couchStructure ]

    let result = validateContext context

    match result with
    | Ok validatedContext ->
        let couchSurface = validatedContext.CouchSurface |> Option.defaultWith (fun () -> failwith "Expected couch surface to be discovered.")
        Assert.Equal("CouchSurface", couchSurface.StructureId)
    | Error errors ->
        failwith (String.concat "; " errors)

[<Fact>]
let ``Context validation filters setup fields from treatment beams`` () =
    let context = createContextWithPlan planWithSetupAndTreatmentBeams [ bodyStructure ]

    let result = validateContext context

    match result with
    | Ok validatedContext ->
        Assert.Single(validatedContext.TreatmentBeams) |> ignore
        Assert.Equal("Beam-1", validatedContext.TreatmentBeams.Head.BeamId)
    | Error errors ->
        failwith (String.concat "; " errors)

[<Fact>]
let ``Context validation fails when no treatment beams remain after setup filtering`` () =
    let context = createContextWithPlan planWithSetupBeamsOnly [ bodyStructure ]

    let result = validateContext context

    match result with
    | Ok _ -> failwith "Expected validation to fail when only setup fields are present."
    | Error errors ->
        Assert.Contains("No treatment beams were supplied.", errors)

[<Fact>]
let ``Creating a detached request requires BODY contour slices for first-version analysis`` () =
    let context = createContext [ meshOnlyBodyStructure ]

    let result =
        context
        |> validateContext
        |> Result.mapError (String.concat "; ")
        |> Result.bind createDetachedCollisionRunRequest

    match result with
    | Ok _ -> failwith "Expected BODY contour extraction to fail."
    | Error error ->
        Assert.Equal("BODY structure does not contain contour slices for first-version analysis.", error)

[<Fact>]
let ``Detached export request obfuscates the patient id and includes the optional couch surface`` () =
    let context = createContext [ bodyStructure; couchStructure ]

    let result =
        context
        |> validateContext
        |> Result.mapError (String.concat "; ")
        |> Result.bind createDetachedCollisionRunRequest

    match result with
    | Ok request ->
        Assert.NotEqual<string>("PATIENT-001", request.Plan.PatientId)
        Assert.Equal("BODY", request.Body.StructureId)
        Assert.Single(request.Accessories) |> ignore
        Assert.Equal(CouchSurface, request.Accessories.Head.Kind)
    | Error error ->
        failwith error
