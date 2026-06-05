module RunStore.Tests

open Shared
open RunStore
open Xunit

let point3D x y z : Point3D = { X = x; Y = y; Z = z }

let point2D x y : Point2D = { X = x; Y = y }

let contourAt z =
    {
        Points = [ point3D 0.0 0.0 z; point3D 10.0 0.0 z; point3D 10.0 10.0 z; point3D 0.0 10.0 z ]
        Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 }
    }

let sampleMesh =
    {
        Vertices = [ point3D 0.0 0.0 0.0; point3D 1.0 0.0 0.0; point3D 0.0 1.0 0.0 ]
        Triangles = [ { A = 0; B = 1; C = 2 } ]
        Bounds = Some { Min = point3D 0.0 0.0 0.0; Max = point3D 1.0 1.0 0.0 }
    }

let couchSurfaceAccessory =
    {
        AccessoryId = "CouchSurface"
        Kind = CouchSurface
        DisplayName = "Couch Surface"
        Mesh = None
        Structure = None
        Bounds = Some { Min = point3D -5.0 -5.0 -1.0; Max = point3D 15.0 15.0 1.0 }
        Offset = None
        IsEnabled = true
    }

let sampleRequest =
    {
        Plan =
            {
                PatientId = "patient"
                CourseId = Some "course"
                StructureSetId = Some "ss"
                PlanId = "plan"
                PlanName = Some "Plan"
                Beams =
                    [
                        {
                            BeamId = "Beam-1"
                            BeamName = Some "Beam 1"
                            BeamKind = TreatmentBeam
                            IsTreatmentBeam = true
                            IsSetupField = false
                            GantryDirection = NotSpecified
                            GantryStart = Some 0.0
                            GantryStop = Some 0.0
                            CouchAngle = Some 0.0
                            PatientSupportAngle = Some 0.0
                            CollimatorAngle = Some 0.0
                            Isocenter = Some(point3D 5.0 5.0 0.0)
                            SourcePosition = Some(point3D 5.0 1005.0 0.0)
                            ControlPoints =
                                [
                                    {
                                        Index = 0
                                        GantryAngle = 0.0
                                        CouchAngle = Some 0.0
                                        CollimatorAngle = Some 0.0
                                        SourcePosition = Some(point3D 5.0 1005.0 0.0)
                                        Isocenter = Some(point3D 5.0 5.0 0.0)
                                        MetersetWeight = Some 0.0
                                        PatientSupportAngle = Some 0.0
                                    }
                                ]
                        }
                    ]
            }
        Body =
            {
                StructureId = "BODY"
                DisplayName = Some "External"
                Mesh = None
                ContourSlices =
                    [
                        {
                            Z = 0.0
                            Contours = [ contourAt 0.0 ]
                            Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 }
                        }
                    ]
                Bounds = Some { Min = point3D 0.0 0.0 -1.0; Max = point3D 10.0 10.0 1.0 }
                SliceThicknessMm = Some 5.0
            }
        SamplingSettings =
            {
                BodySampleStepMm = Some 2.5
                BeamSampleStepMm = Some 1.0
                BeamAxisOffsetMm = Some 0.0
                ClearanceRadiusMm = Some 1.0
                ClearanceDistanceMm = Some 0.0
                CollisionToleranceMm = Some 1.0
                ArcStepDegrees = Some 1.0
            }
        Accessories = []
    }

[<Fact>]
let ``Creating a run stores the request`` () =
    let response = createRun sampleRequest

    match response with
    | Ok createdRun ->
        let storedRun = tryGetRun createdRun.RunId

        match storedRun with
        | Some runRecord -> Assert.Equal(sampleRequest, runRecord.Request)
        | None -> failwith "Expected the created collision run to be stored."
    | Error error ->
        failwith error

[<Fact>]
let ``Creating a run returns a run id`` () =
    let response = createRun sampleRequest

    match response with
    | Ok createdRun ->
        Assert.NotEqual(System.Guid.Empty, createdRun.RunId)
    | Error error ->
        failwith error

[<Fact>]
let ``A stored run can be retrieved`` () =
    let response = createRun sampleRequest

    match response with
    | Ok createdRun ->
        let storedRun = tryGetRun createdRun.RunId
        Assert.True(storedRun.IsSome)
    | Error error ->
        failwith error

[<Fact>]
let ``Default analysis is executed for a created run`` () =
    let response = createRun sampleRequest

    match response with
    | Ok createdRun ->
        Assert.True(createdRun.Summary.FlatResult.IsSome)
        Assert.Equal(CollisionDetected, createdRun.Summary.Status)
    | Error error ->
        failwith error

[<Fact>]
let ``Creating a run with BODY succeeds`` () =
    let response = createRun sampleRequest

    Assert.True(Result.isOk response)

[<Fact>]
let ``Creating a run without BODY fails`` () =
    let invalidRequest = { sampleRequest with Body = { sampleRequest.Body with StructureId = "" } }

    let response = createRun invalidRequest

    match response with
    | Ok _ -> failwith "Expected BODY validation to fail."
    | Error error -> Assert.Equal("Collision run request is missing BODY.", error)

[<Fact>]
let ``Creating a run without couch succeeds`` () =
    let requestWithoutCouch = { sampleRequest with Accessories = [] }

    let response = createRun requestWithoutCouch

    Assert.True(Result.isOk response)

[<Fact>]
let ``Creating a run with BODY contour slices is accepted`` () =
    let response = createRun sampleRequest

    Assert.True(Result.isOk response)

[<Fact>]
let ``Creating a run with a mesh-only BODY is rejected for first-version analysis`` () =
    let meshOnlyBody =
        {
            sampleRequest.Body with
                Mesh = Some sampleMesh
                ContourSlices = []
                Bounds = sampleMesh.Bounds
        }

    let response = createRun { sampleRequest with Body = meshOnlyBody }

    match response with
    | Ok _ -> failwith "Expected mesh-only BODY validation to fail."
    | Error error ->
        Assert.Equal("Collision run request BODY structure must include contour slices for first-version analysis.", error)

[<Fact>]
let ``Creating a run can include an optional couch surface without requiring it`` () =
    let response = createRun { sampleRequest with Accessories = [ couchSurfaceAccessory ] }

    Assert.True(Result.isOk response)
