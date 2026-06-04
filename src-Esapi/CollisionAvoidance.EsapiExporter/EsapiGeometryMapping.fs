module CollisionAvoidance.EsapiExporter.EsapiGeometryMapping

open Shared

type VectorLike = {
    X: float
    Y: float
    Z: float
}

type Rect3DLike = {
    X: float
    Y: float
    Z: float
    SizeX: float
    SizeY: float
    SizeZ: float
}

type MeshGeometryLike = {
    Vertices: VectorLike list
    TriangleIndices: int list
    Bounds: Rect3DLike option
    CanFreeze: bool
}

type ContourPointLike = {
    X: float
    Y: float
    Z: float
}

type ContourSliceLike = {
    Z: float
    Contours: ContourPointLike list list
}

type DetachedMeshSnapshot =
    private
        {
            Mesh: MeshGeometryLike
        }

/// Maps an ESAPI VVector-like value into a detached Point3D contract.
let mapVVectorToPoint3D (vector: VectorLike) : Point3D = {
    X = vector.X
    Y = vector.Y
    Z = vector.Z
}

/// Maps a contour point-like value into a detached Point3D contract.
let mapContourPointToPoint3D (point: ContourPointLike) : Point3D = {
    X = point.X
    Y = point.Y
    Z = point.Z
}

/// Maps a contour loop into a detached contour DTO with simple 2D bounds.
let mapContourToContourDto (points: ContourPointLike list) : ContourDto =
    let mappedPoints = points |> List.map mapContourPointToPoint3D

    let bounds : Bounds2D option =
        match mappedPoints with
        | [] -> None
        | head :: tail ->
            let allPoints = head :: tail
            let xs = allPoints |> List.map (fun point -> point.X)
            let ys = allPoints |> List.map (fun point -> point.Y)

            Some ({
                Min = ({ X = List.min xs; Y = List.min ys }: Point2D)
                Max = ({ X = List.max xs; Y = List.max ys }: Point2D)
            }: Bounds2D)

    {
        Points = mappedPoints
        Bounds = bounds
    }

/// Maps ESAPI mesh bounds into a detached Bounds3D contract.
let mapMeshBoundsToBounds3D (meshBounds: Rect3DLike) : Bounds3D = {
    Min = { X = meshBounds.X; Y = meshBounds.Y; Z = meshBounds.Z }
    Max = {
        X = meshBounds.X + meshBounds.SizeX
        Y = meshBounds.Y + meshBounds.SizeY
        Z = meshBounds.Z + meshBounds.SizeZ
    }
}

/// Creates a detached mesh snapshot mirroring the prototype clone-and-freeze boundary.
let createDetachedMeshSnapshot (meshGeometry: MeshGeometryLike) : Result<DetachedMeshSnapshot, string> =
    if meshGeometry.CanFreeze then
        Ok { Mesh = meshGeometry }
    else
        Error "Body mesh clone could not be frozen."

/// Gets the detached mesh payload from a mesh snapshot wrapper.
let getDetachedMeshValue (snapshot: DetachedMeshSnapshot) : MeshGeometryLike =
    snapshot.Mesh

/// Maps ESAPI mesh geometry into a detached MeshDto contract.
let mapMeshToMeshDto (meshGeometry: MeshGeometryLike) : Result<MeshDto, string> =
    if meshGeometry.TriangleIndices.Length % 3 <> 0 then
        Error "Triangle indices must be a multiple of three."
    else
        Ok {
            Vertices = meshGeometry.Vertices |> List.map mapVVectorToPoint3D
            Triangles =
                meshGeometry.TriangleIndices
                |> List.chunkBySize 3
                |> List.map (fun triangle -> {
                    A = triangle[0]
                    B = triangle[1]
                    C = triangle[2]
                })
            Bounds = meshGeometry.Bounds |> Option.map mapMeshBoundsToBounds3D
        }

/// Maps one ESAPI contour slice into a detached ContourSliceDto contract.
let mapContourSlice (slice: ContourSliceLike) : ContourSliceDto =
    let contours = slice.Contours |> List.map mapContourToContourDto

    let bounds : Bounds2D option =
        contours
        |> List.choose (fun contour -> contour.Bounds)
        |> function
            | [] -> None
            | contourBounds ->
                let minXs = contourBounds |> List.map (fun bounds -> bounds.Min.X)
                let minYs = contourBounds |> List.map (fun bounds -> bounds.Min.Y)
                let maxXs = contourBounds |> List.map (fun bounds -> bounds.Max.X)
                let maxYs = contourBounds |> List.map (fun bounds -> bounds.Max.Y)

                Some ({
                    Min = ({ X = List.min minXs; Y = List.min minYs }: Point2D)
                    Max = ({ X = List.max maxXs; Y = List.max maxYs }: Point2D)
                }: Bounds2D)

    {
        Z = slice.Z
        Contours = contours
        Bounds = bounds
    }

/// Maps one ESAPI contour slice into a detached BODY slice DTO contract.
let mapBodySlice (slice: ContourSliceLike) : BodySliceDto =
    let contourSlice = mapContourSlice slice

    {
        Z = contourSlice.Z
        Contours = contourSlice.Contours
        Bounds = contourSlice.Bounds
    }
