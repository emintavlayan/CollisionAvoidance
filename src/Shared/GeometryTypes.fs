namespace Shared

type Point3D = {
    X: float
    Y: float
    Z: float
}

type Vector3D = {
    X: float
    Y: float
    Z: float
}

type Bounds3D = {
    Min: Point3D
    Max: Point3D
}

type TriangleIndex = {
    A: int
    B: int
    C: int
}

type MeshDto = {
    Vertices: Point3D list
    Triangles: TriangleIndex list
    Bounds: Bounds3D option
}

type ContourSliceDto = {
    Z: float
    Contours: Point3D list list
}
