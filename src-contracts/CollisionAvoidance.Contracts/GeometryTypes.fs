namespace Shared

type Point2D = {
    X: float
    Y: float
}

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

type Bounds2D = {
    Min: Point2D
    Max: Point2D
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

type ContourDto = {
    Points: Point3D list
    Bounds: Bounds2D option
}

type ContourSliceDto = {
    Z: float
    Contours: ContourDto list
    Bounds: Bounds2D option
}

type BodySliceDto = {
    Z: float
    Contours: ContourDto list
    Bounds: Bounds2D option
}
