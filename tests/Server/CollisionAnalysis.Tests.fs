module CollisionAnalysis.Tests

open Shared
open BodyVolume
open ClearanceSampling
open CollisionAnalysis
open Xunit

let point3D x y z : Point3D = { X = x; Y = y; Z = z }

let point2D x y : Point2D = { X = x; Y = y }

let squareContour z =
    {
        Points = [ point3D 0.0 0.0 z; point3D 10.0 0.0 z; point3D 10.0 10.0 z; point3D 0.0 10.0 z ]
        Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 }
    }

let sampleBody =
    {
        StructureId = "BODY"
        DisplayName = Some "External"
        Mesh = None
        ContourSlices = [ { Z = 0.0; Contours = [ squareContour 0.0 ]; Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 } } ]
        Bounds = Some { Min = point3D 0.0 0.0 -1.0; Max = point3D 10.0 10.0 1.0 }
        SliceThicknessMm = Some 5.0
    }

let sampleSettings =
    {
        BodySampleStepMm = Some 2.5
        BeamSampleStepMm = Some 1.0
        BeamAxisOffsetMm = Some 0.0
        ClearanceRadiusMm = Some 1.0
        ClearanceDistanceMm = Some 0.0
        CollisionToleranceMm = Some 1.0
        ArcStepDegrees = Some 1.0
    }

let sampleBeam =
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

let outsideBody =
    {
        sampleBody with
            Bounds = Some { Min = point3D 100.0 100.0 -1.0; Max = point3D 110.0 110.0 1.0 }
            ContourSlices = [ { Z = 0.0; Contours = [ squareContour 0.0 |> fun contour -> { contour with Points = contour.Points |> List.map (fun point -> point3D (point.X + 100.0) (point.Y + 100.0) point.Z) } ]; Bounds = Some { Min = point2D 100.0 100.0; Max = point2D 110.0 110.0 } } ]
    }

[<Fact>]
let ``A point inside a square contour is classified as inside`` () =
    let isInside = isPointInsideBody sampleBody (point3D 5.0 5.0 0.0)

    Assert.True(isInside)

[<Fact>]
let ``A point outside a square contour is classified as outside`` () =
    let isInside = isPointInsideBody sampleBody (point3D 15.0 15.0 0.0)

    Assert.False(isInside)

[<Fact>]
let ``A point outside the body bounds is filtered before volume checking`` () =
    let candidates =
        [
            { Location = point3D 5.0 5.0 0.0; Source = { BeamId = "Beam-1"; ControlPointIndex = Some 0; GantryAngle = Some 0.0; SampleType = LineSample; AccessoryId = None } }
            { Location = point3D 50.0 50.0 0.0; Source = { BeamId = "Beam-1"; ControlPointIndex = Some 0; GantryAngle = Some 0.0; SampleType = LineSample; AccessoryId = None } }
        ]

    let boundedCandidates = filterCandidatesByBounds sampleBody candidates

    Assert.Single(boundedCandidates) |> ignore

[<Fact>]
let ``Clearance sampling creates points for a beam`` () =
    let candidates =
        generateClearanceSamplePointsForBeam sampleSettings sampleBeam
        |> Result.defaultValue []

    Assert.NotEmpty(candidates)

[<Fact>]
let ``A flat analysis reports no collision when all points are outside`` () =
    let request = { Plan = { PatientId = "patient"; CourseId = Some "course"; StructureSetId = Some "ss"; PlanId = "plan"; PlanName = Some "Plan"; Beams = [ sampleBeam ] }; Body = outsideBody; SamplingSettings = sampleSettings; Accessories = [] }

    let result = createFlatResult request

    match result with
    | Ok flatResult ->
        Assert.Equal(NoCollision, flatResult.Status)
    | Error error ->
        failwith error

[<Fact>]
let ``A flat analysis reports collision when one point is inside`` () =
    let request = { Plan = { PatientId = "patient"; CourseId = Some "course"; StructureSetId = Some "ss"; PlanId = "plan"; PlanName = Some "Plan"; Beams = [ sampleBeam ] }; Body = sampleBody; SamplingSettings = sampleSettings; Accessories = [] }

    let result = createFlatResult request

    match result with
    | Ok flatResult ->
        Assert.Equal(CollisionDetected, flatResult.Status)
    | Error error ->
        failwith error

[<Fact>]
let ``Detailed analysis keeps beam id on collision point`` () =
    let request = { Plan = { PatientId = "patient"; CourseId = Some "course"; StructureSetId = Some "ss"; PlanId = "plan"; PlanName = Some "Plan"; Beams = [ sampleBeam ] }; Body = sampleBody; SamplingSettings = sampleSettings; Accessories = [] }

    let result = createDetailedAnalysis request

    match result with
    | Ok detailedResult ->
        let collisionPoint = detailedResult.CollisionPoints |> List.head
        let source = collisionPoint.Source |> Option.defaultWith (fun () -> failwith "Expected detailed analysis to keep point provenance.")
        Assert.Equal("Beam-1", source.BeamId)
    | Error error ->
        failwith error

[<Fact>]
let ``Detailed analysis keeps control point index when available`` () =
    let request = { Plan = { PatientId = "patient"; CourseId = Some "course"; StructureSetId = Some "ss"; PlanId = "plan"; PlanName = Some "Plan"; Beams = [ sampleBeam ] }; Body = sampleBody; SamplingSettings = sampleSettings; Accessories = [] }

    let result = createDetailedAnalysis request

    match result with
    | Ok detailedResult ->
        let collisionPoint = detailedResult.CollisionPoints |> List.head
        let source = collisionPoint.Source |> Option.defaultWith (fun () -> failwith "Expected detailed analysis to keep point provenance.")
        Assert.Equal(Some 0, source.ControlPointIndex)
    | Error error ->
        failwith error

[<Fact>]
let ``Detailed analysis can group result by beam`` () =
    let secondBeam =
        {
            sampleBeam with
                BeamId = "Beam-2"
                BeamName = Some "Beam 2"
                ControlPoints = [ { sampleBeam.ControlPoints.Head with Index = 1 } ]
        }

    let request =
        {
            Plan =
                {
                    PatientId = "patient"
                    CourseId = Some "course"
                    StructureSetId = Some "ss"
                    PlanId = "plan"
                    PlanName = Some "Plan"
                    Beams = [ sampleBeam; secondBeam ]
                }
            Body = sampleBody
            SamplingSettings = sampleSettings
            Accessories = []
        }

    let result = createDetailedAnalysis request

    match result with
    | Ok detailedResult ->
        Assert.Equal(2, detailedResult.BeamResults.Length)
        Assert.Contains(detailedResult.BeamResults, fun beamResult -> beamResult.BeamId = "Beam-1")
        Assert.Contains(detailedResult.BeamResults, fun beamResult -> beamResult.BeamId = "Beam-2")
    | Error error ->
        failwith error
