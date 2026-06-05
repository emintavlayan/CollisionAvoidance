module EsapiExporter.Tests

open System.Windows.Media.Media3D
open CollisionAvoidance.EsapiExporter.ContextValidation
open CollisionAvoidance.EsapiExporter.EsapiGeometryMapping
open CollisionAvoidance.EsapiExporter.EsapiPlanExtraction
open CollisionAvoidance.EsapiExporter.PatientIdObfuscation
open Shared
open VMS.TPS.Common.Model.Types
open Xunit

/// Represents a detached 3D point used by the ESAPI exporter tests.
let point3D x y z : Shared.Point3D = { X = x; Y = y; Z = z }

/// Represents a detached 2D point used by the ESAPI exporter tests.
let point2D x y : Point2D = { X = x; Y = y }

[<Fact>]
let ``Patient id obfuscation is deterministic for the same raw id`` () =
    let rawId = "PATIENT-001"

    let firstHash = obfuscatePatientId rawId
    let secondHash = obfuscatePatientId rawId

    Assert.Equal(firstHash, secondHash)
    Assert.NotEqual<string>(rawId, firstHash)

[<Fact>]
let ``BODY identifier matching is case insensitive`` () =
    Assert.True(isBodyStructureId "body")
    Assert.True(isBodyStructureId "BODY")
    Assert.False(isBodyStructureId "PTV")

[<Fact>]
let ``Couch surface detection accepts exact ids and couch-like display names`` () =
    Assert.True(looksLikeCouchSurfaceIdentifier "CouchSurface" None)
    Assert.True(looksLikeCouchSurfaceIdentifier "SUPPORT" (Some "Couch Surface"))
    Assert.False(looksLikeCouchSurfaceIdentifier "PTV" (Some "Target"))

[<Fact>]
let ``Default sampling settings preserve the first-version millimeter values`` () =
    Assert.Equal(2.5, defaultSamplingSettings.BodySampleStepMm.Value |> Length.toFloatMm)
    Assert.Equal(5.0, defaultSamplingSettings.BeamSampleStepMm.Value |> Length.toFloatMm)
    Assert.Equal(550.0, defaultSamplingSettings.BeamAxisOffsetMm.Value |> Length.toFloatMm)
    Assert.Equal(390.0, defaultSamplingSettings.ClearanceRadiusMm.Value |> Length.toFloatMm)

[<Fact>]
let ``Real ESAPI gantry directions map into detached direction values`` () =
    Assert.Equal(Clockwise, mapGantryDirection GantryDirection.Clockwise)
    Assert.Equal(CounterClockwise, mapGantryDirection GantryDirection.CounterClockwise)
    Assert.Equal(NotSpecified, mapGantryDirection GantryDirection.None)

[<Fact>]
let ``Mapping a VVector creates a detached point`` () =
    let point = mapVVectorToPoint3D (VVector(1.0, 2.0, 3.0))

    Assert.Equal(1.0, point.X)
    Assert.Equal(2.0, point.Y)
    Assert.Equal(3.0, point.Z)

[<Fact>]
let ``Mapping contour slices computes contour and slice bounds`` () =
    let slice =
        mapBodySlice
            5.0
            [|
                [|
                    VVector(0.0, 0.0, 5.0)
                    VVector(10.0, 0.0, 5.0)
                    VVector(10.0, 10.0, 5.0)
                    VVector(0.0, 10.0, 5.0)
                |]
            |]

    Assert.Equal(5.0, slice.Z)
    Assert.Single(slice.Contours) |> ignore
    Assert.Equal(0.0, slice.Bounds.Value.Min.X)
    Assert.Equal(10.0, slice.Bounds.Value.Max.Y)

[<Fact>]
let ``Creating a detached mesh snapshot clones and freezes a mesh`` () =
    let mesh = MeshGeometry3D()
    mesh.Positions.Add(System.Windows.Media.Media3D.Point3D(0.0, 0.0, 0.0))
    mesh.Positions.Add(System.Windows.Media.Media3D.Point3D(1.0, 0.0, 0.0))
    mesh.Positions.Add(System.Windows.Media.Media3D.Point3D(0.0, 1.0, 0.0))
    mesh.TriangleIndices.Add(0)
    mesh.TriangleIndices.Add(1)
    mesh.TriangleIndices.Add(2)

    let result = createDetachedMeshSnapshot mesh

    match result with
    | Ok snapshot ->
        let frozenMesh = getDetachedMeshValue snapshot
        Assert.True(frozenMesh.IsFrozen)
        Assert.NotSame(mesh, frozenMesh)
    | Error error ->
        failwith error

[<Fact>]
let ``Mapping a mesh creates vertices triangles and bounds`` () =
    let mesh = MeshGeometry3D()
    mesh.Positions.Add(System.Windows.Media.Media3D.Point3D(0.0, 0.0, 0.0))
    mesh.Positions.Add(System.Windows.Media.Media3D.Point3D(1.0, 0.0, 0.0))
    mesh.Positions.Add(System.Windows.Media.Media3D.Point3D(0.0, 1.0, 0.0))
    mesh.TriangleIndices.Add(0)
    mesh.TriangleIndices.Add(1)
    mesh.TriangleIndices.Add(2)

    let result = mapMeshToMeshDto mesh

    match result with
    | Ok dto ->
        Assert.Equal(3, dto.Vertices.Length)
        Assert.Single(dto.Triangles) |> ignore
        Assert.True(dto.Bounds.IsSome)
    | Error error ->
        failwith error

[<Fact>]
let ``Combining body slice bounds creates detached volume bounds`` () =
    let slices =
        [
            {
                Z = 0.0
                Contours = []
                Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 }
            }
            {
                Z = 5.0
                Contours = []
                Bounds = Some { Min = point2D -2.0 -1.0; Max = point2D 12.0 11.0 }
            }
        ]

    let bounds = tryCreateBounds3DFromBodySlices slices

    Assert.True(bounds.IsSome)
    Assert.Equal(-2.0, bounds.Value.Min.X)
    Assert.Equal(11.0, bounds.Value.Max.Y)
    Assert.Equal(5.0, bounds.Value.Max.Z)
