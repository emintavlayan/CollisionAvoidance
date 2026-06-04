module EsapiExporter.Tests

open CollisionAvoidance.EsapiExporter.PatientIdObfuscation
open CollisionAvoidance.EsapiExporter.EsapiGeometryMapping
open Shared
open Xunit

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
