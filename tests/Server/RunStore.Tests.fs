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
    let storedRun = tryGetRun response.RunId

    match storedRun with
    | Some runRecord -> Assert.Equal(sampleRequest, runRecord.Request)
    | None -> failwith "Expected the created collision run to be stored."

[<Fact>]
let ``Creating a run returns a run id`` () =
    let response = createRun sampleRequest

    Assert.NotEqual(System.Guid.Empty, response.RunId)

[<Fact>]
let ``A stored run can be retrieved`` () =
    let response = createRun sampleRequest
    let storedRun = tryGetRun response.RunId

    Assert.True(storedRun.IsSome)

[<Fact>]
let ``Default analysis is executed for a created run`` () =
    let response = createRun sampleRequest

    Assert.True(response.Summary.FlatResult.IsSome)
    Assert.Equal(CollisionDetected, response.Summary.Status)
