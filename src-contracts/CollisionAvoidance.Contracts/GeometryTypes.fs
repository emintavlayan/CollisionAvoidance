namespace Shared

/// Represents the centimeter unit used for detached coordinate-system documentation and conversions.
[<Measure>]
type cm

/// Represents the millimeter unit used for sampling, tolerance, and reported physical distances.
[<Measure>]
type mm

/// Represents helper functions for constructing and converting detached length values.
module Length =
    /// Represents a raw floating-point value as centimeters.
    let inline centimeters (value: float) : float<cm> =
        LanguagePrimitives.FloatWithMeasure<cm> value

    /// Represents a raw floating-point value as millimeters.
    let inline millimeters (value: float) : float<mm> =
        LanguagePrimitives.FloatWithMeasure<mm> value

    /// Represents the raw floating-point value behind a centimeter measurement.
    let inline toFloatCm (value: float<cm>) =
        float value

    /// Represents the raw floating-point value behind a millimeter measurement.
    let inline toFloatMm (value: float<mm>) =
        float value

    /// Represents a centimeter value converted from millimeters.
    let inline mmToCm (value: float<mm>) : float<cm> =
        centimeters (toFloatMm value / 10.0)

    /// Represents a millimeter value converted from centimeters.
    let inline cmToMm (value: float<cm>) : float<mm> =
        millimeters (toFloatCm value * 10.0)

/// Represents one detached 2D point whose coordinate values are expressed in centimeters.
type Point2D = {
    X: float
    Y: float
}

/// Represents one detached 3D point whose coordinate values are expressed in centimeters.
type Point3D = {
    X: float
    Y: float
    Z: float
}

/// Represents one detached 3D vector whose components are interpreted in the current geometry workflow context.
type Vector3D = {
    X: float
    Y: float
    Z: float
}

/// Represents detached 2D minimum and maximum extents.
type Bounds2D = {
    Min: Point2D
    Max: Point2D
}

/// Represents detached 3D minimum and maximum extents.
type Bounds3D = {
    Min: Point3D
    Max: Point3D
}

/// Represents one indexed triangle in a detached mesh.
type TriangleIndex = {
    A: int
    B: int
    C: int
}

/// Represents one detached mesh with vertices, triangle indices, and optional bounds.
type MeshDto = {
    Vertices: Point3D list
    Triangles: TriangleIndex list
    Bounds: Bounds3D option
}

/// Represents one detached 2D contour loop on an axial slice.
type ContourDto = {
    Points: Point3D list
    Bounds: Bounds2D option
}

/// Represents one detached structure contour slice positioned along the Z axis in centimeters.
type ContourSliceDto = {
    Z: float
    Contours: ContourDto list
    Bounds: Bounds2D option
}

/// Represents one detached BODY contour slice positioned along the Z axis in centimeters.
type BodySliceDto = {
    Z: float
    Contours: ContourDto list
    Bounds: Bounds2D option
}
