module CollisionAvoidance.EsapiExporter.EsapiGeometryMapping

open System.Windows.Media.Media3D
open Shared
open VMS.TPS.Common.Model.Types

/// Represents a detached wrapper around a cloned and frozen ESAPI mesh snapshot.
type DetachedMeshSnapshot =
    private
        {
            Mesh: MeshGeometry3D
        }

/// Represents detached 2D bounds computed from a point list when points are available.
let tryCreateBounds2D (points: Shared.Point3D list) : Bounds2D option =
    match points with
    | [] -> None
    | _ ->
        Some {
            Min = {
                X = points |> List.map (fun point -> point.X) |> List.min
                Y = points |> List.map (fun point -> point.Y) |> List.min
            }
            Max = {
                X = points |> List.map (fun point -> point.X) |> List.max
                Y = points |> List.map (fun point -> point.Y) |> List.max
            }
        }

/// Represents detached 2D bounds combined from many slice or contour bounds values.
let tryCombineBounds2D (boundsValues: Bounds2D list) : Bounds2D option =
    match boundsValues with
    | [] -> None
    | _ ->
        Some {
            Min = {
                X = boundsValues |> List.map (fun bounds -> bounds.Min.X) |> List.min
                Y = boundsValues |> List.map (fun bounds -> bounds.Min.Y) |> List.min
            }
            Max = {
                X = boundsValues |> List.map (fun bounds -> bounds.Max.X) |> List.max
                Y = boundsValues |> List.map (fun bounds -> bounds.Max.Y) |> List.max
            }
        }

/// Represents detached 3D bounds combined from many `(z, bounds)` pairs.
let tryCreateBounds3DFromSliceBounds (sliceBounds: (float * Bounds2D) list) : Bounds3D option =
    match sliceBounds with
    | [] -> None
    | _ ->
        Some {
            Min = {
                X = sliceBounds |> List.map (fun (_, bounds) -> bounds.Min.X) |> List.min
                Y = sliceBounds |> List.map (fun (_, bounds) -> bounds.Min.Y) |> List.min
                Z = sliceBounds |> List.map fst |> List.min
            }
            Max = {
                X = sliceBounds |> List.map (fun (_, bounds) -> bounds.Max.X) |> List.max
                Y = sliceBounds |> List.map (fun (_, bounds) -> bounds.Max.Y) |> List.max
                Z = sliceBounds |> List.map fst |> List.max
            }
        }

/// Represents detached 3D bounds derived from BODY contour slices.
let tryCreateBounds3DFromBodySlices (slices: BodySliceDto list) =
    slices
    |> List.choose (fun slice -> slice.Bounds |> Option.map (fun bounds -> slice.Z, bounds))
    |> tryCreateBounds3DFromSliceBounds

/// Represents detached 3D bounds derived from general structure contour slices.
let tryCreateBounds3DFromContourSlices (slices: ContourSliceDto list) =
    slices
    |> List.choose (fun slice -> slice.Bounds |> Option.map (fun bounds -> slice.Z, bounds))
    |> tryCreateBounds3DFromSliceBounds

/// Represents one ESAPI `VVector` mapped into a detached point DTO.
let mapVVectorToPoint3D (vector: VVector) : Shared.Point3D = {
    X = vector.x
    Y = vector.y
    Z = vector.z
}

/// Represents one WPF `Rect3D` bounds value mapped into a detached 3D-bounds DTO.
let mapMeshBoundsToBounds3D (meshBounds: Rect3D) : Bounds3D = {
    Min = { X = meshBounds.X; Y = meshBounds.Y; Z = meshBounds.Z }
    Max = {
        X = meshBounds.X + meshBounds.SizeX
        Y = meshBounds.Y + meshBounds.SizeY
        Z = meshBounds.Z + meshBounds.SizeZ
    }
}

/// Represents one ESAPI contour loop mapped into a detached contour DTO.
let mapContourToContourDto (points: VVector array) : ContourDto =
    let mappedPoints = points |> Array.toList |> List.map mapVVectorToPoint3D

    {
        Points = mappedPoints
        Bounds = tryCreateBounds2D mappedPoints
    }

/// Represents one ESAPI contour slice mapped into a detached structure-slice DTO.
let mapContourSlice (z: float) (contours: VVector array array) : ContourSliceDto =
    let contourDtos =
        contours
        |> Array.toList
        |> List.filter (fun contour -> contour.Length > 0)
        |> List.map mapContourToContourDto

    {
        Z = z
        Contours = contourDtos
        Bounds = contourDtos |> List.choose (fun contour -> contour.Bounds) |> tryCombineBounds2D
    }

/// Represents one ESAPI contour slice mapped into a detached BODY-slice DTO.
let mapBodySlice (z: float) (contours: VVector array array) : BodySliceDto =
    let contourSlice = mapContourSlice z contours

    {
        Z = contourSlice.Z
        Contours = contourSlice.Contours
        Bounds = contourSlice.Bounds
    }

/// Represents a detached mesh snapshot created from a cloned and frozen ESAPI mesh.
let createDetachedMeshSnapshot (meshGeometry: MeshGeometry3D) : Result<DetachedMeshSnapshot, string> =
    if isNull meshGeometry then
        Error "Body mesh was null."
    else
        let clonedMesh = meshGeometry.Clone()

        if clonedMesh.CanFreeze then
            clonedMesh.Freeze()
            Ok { Mesh = clonedMesh }
        else
            Error "Body mesh clone could not be frozen."

/// Represents the frozen detached mesh value stored inside a mesh snapshot wrapper.
let getDetachedMeshValue (snapshot: DetachedMeshSnapshot) =
    snapshot.Mesh

/// Represents one ESAPI mesh mapped into a detached mesh DTO.
let mapMeshToMeshDto (meshGeometry: MeshGeometry3D) : Result<MeshDto, string> =
    if meshGeometry.TriangleIndices.Count % 3 <> 0 then
        Error "Triangle indices must be a multiple of three."
    else
        Ok {
            Vertices =
                meshGeometry.Positions
                |> Seq.cast<System.Windows.Media.Media3D.Point3D>
                |> Seq.map (fun point -> ({ X = point.X; Y = point.Y; Z = point.Z }: Shared.Point3D))
                |> Seq.toList
            Triangles =
                meshGeometry.TriangleIndices
                |> Seq.cast<int>
                |> Seq.toList
                |> List.chunkBySize 3
                |> List.map (fun triangle -> {
                    A = triangle[0]
                    B = triangle[1]
                    C = triangle[2]
                })
            Bounds = Some (meshGeometry.Bounds |> mapMeshBoundsToBounds3D)
        }
