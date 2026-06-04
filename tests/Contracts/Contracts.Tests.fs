module Contracts.Tests

open System
open Shared
open Xunit

let samplePoint x y z : Point3D = { X = x; Y = y; Z = z }

let samplePoint2D x y : Point2D = { X = x; Y = y }

let sampleContour =
    {
        Points = [ samplePoint 0.0 0.0 0.0; samplePoint 1.0 0.0 0.0; samplePoint 1.0 1.0 0.0 ]
        Bounds = Some { Min = samplePoint2D 0.0 0.0; Max = samplePoint2D 1.0 1.0 }
    }

let sampleBody =
    {
        StructureId = "BODY"
        DisplayName = Some "External"
        Mesh = None
        ContourSlices = [ { Z = 0.0; Contours = [ sampleContour ]; Bounds = sampleContour.Bounds } ]
        Bounds = Some { Min = samplePoint 0.0 0.0 0.0; Max = samplePoint 10.0 10.0 10.0 }
        SliceThicknessMm = Some 2.5
    }

let sampleBeam =
    {
        BeamId = "B1"
        BeamName = Some "Arc 1"
        BeamKind = TreatmentBeam
        IsTreatmentBeam = true
        IsSetupField = false
        GantryDirection = Clockwise
        GantryStart = Some 180.0
        GantryStop = Some 90.0
        CouchAngle = Some 0.0
        PatientSupportAngle = Some 0.0
        CollimatorAngle = Some 15.0
        Isocenter = Some(samplePoint 0.0 0.0 0.0)
        SourcePosition = Some(samplePoint 0.0 1000.0 0.0)
        ControlPoints =
            [
                {
                    Index = 0
                    GantryAngle = 180.0
                    CouchAngle = Some 0.0
                    CollimatorAngle = Some 15.0
                    SourcePosition = Some(samplePoint 0.0 1000.0 0.0)
                    Isocenter = Some(samplePoint 0.0 0.0 0.0)
                    MetersetWeight = Some 0.0
                    PatientSupportAngle = Some 0.0
                }
            ]
    }

[<Fact>]
let ``A bounds record can represent distinct minimum and maximum points`` () =
    let bounds = { Min = samplePoint -1.0 -2.0 -3.0; Max = samplePoint 4.0 5.0 6.0 }

    Assert.Equal(-1.0, bounds.Min.X)
    Assert.Equal(6.0, bounds.Max.Z)

[<Fact>]
let ``A sample detailed collision DTO can be constructed`` () =
    let collisionPoint = {
        Location = samplePoint 1.0 2.0 3.0
        DistanceMm = Some 0.5
        Description = Some "Inside BODY"
        Source =
            Some {
                BeamId = "B1"
                ControlPointIndex = Some 0
                GantryAngle = Some 180.0
                SampleType = LineSample
            }
    }

    let detailedResult = {
        Status = CollisionDetected
        BeamResults =
            [
                {
                    BeamId = "B1"
                    BeamName = Some "Arc 1"
                    Status = CollisionDetected
                    ControlPointResults =
                        [ { ControlPointIndex = 0; GantryAngle = Some 180.0; Status = CollisionDetected; CollisionPoints = [ collisionPoint ] } ]
                }
            ]
        ControlPointResults = [ { ControlPointIndex = 0; GantryAngle = Some 180.0; Status = CollisionDetected; CollisionPoints = [ collisionPoint ] } ]
        CollisionPoints = [ collisionPoint ]
    }

    Assert.Single(detailedResult.BeamResults) |> ignore
    Assert.Equal(CollisionDetected, detailedResult.Status)

[<Fact>]
let ``A collision run request can contain a body snapshot and beam snapshots`` () =
    let request = {
        Plan =
            {
                PatientId = "hashed-patient"
                CourseId = Some "C1"
                StructureSetId = Some "SS1"
                PlanId = "P1"
                PlanName = Some "Plan 1"
                Beams = [ sampleBeam ]
            }
        Body = sampleBody
        SamplingSettings =
            {
                BodySampleStepMm = Some 2.5
                BeamSampleStepMm = Some 5.0
                ClearanceDistanceMm = Some 390.0
                CollisionToleranceMm = Some 1.0
                ArcStepDegrees = Some 1.0
            }
        Accessories = []
    }

    Assert.Single(request.Plan.Beams) |> ignore
    Assert.Equal("BODY", request.Body.StructureId)
