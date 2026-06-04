module CollisionAvoidance.EsapiExporter.EsapiGeometryMapping

open Shared

/// Maps an ESAPI VVector-like value into a detached Point3D contract.
let mapVVectorToPoint3D (_vector: obj) : Point3D =
    failwith "TODO: Map ESAPI VVector values into Point3D."

/// Maps ESAPI mesh bounds into a detached Bounds3D contract.
let mapMeshBoundsToBounds3D (_meshBounds: obj) : Bounds3D =
    failwith "TODO: Map ESAPI mesh bounds into Bounds3D."

/// Maps ESAPI mesh geometry into a detached MeshDto contract.
let mapMeshToMeshDto (_meshGeometry: obj) : MeshDto =
    failwith "TODO: Clone, freeze, and map ESAPI mesh geometry into MeshDto."

/// Maps one ESAPI contour slice into a detached ContourSliceDto contract.
let mapContourSlice (_slice: obj) : ContourSliceDto =
    failwith "TODO: Map one ESAPI contour slice into ContourSliceDto."
