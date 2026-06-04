module GeometryMath

open Shared

/// Creates a vector that points from one point to another.
let vectorBetween (startPoint: Point3D) (endPoint: Point3D) : Vector3D = {
    X = endPoint.X - startPoint.X
    Y = endPoint.Y - startPoint.Y
    Z = endPoint.Z - startPoint.Z
}

/// Adds two vectors component-wise.
let addVectors (left: Vector3D) (right: Vector3D) : Vector3D = {
    X = left.X + right.X
    Y = left.Y + right.Y
    Z = left.Z + right.Z
}

/// Subtracts the right vector from the left vector component-wise.
let subtractVectors (left: Vector3D) (right: Vector3D) : Vector3D = {
    X = left.X - right.X
    Y = left.Y - right.Y
    Z = left.Z - right.Z
}

/// Scales a vector by a scalar value.
let scaleVector (scalar: float) (vector: Vector3D) : Vector3D = {
    X = vector.X * scalar
    Y = vector.Y * scalar
    Z = vector.Z * scalar
}

/// Computes the dot product of two vectors.
let dotProduct (left: Vector3D) (right: Vector3D) =
    left.X * right.X + left.Y * right.Y + left.Z * right.Z

/// Computes the cross product of two vectors.
let crossProduct (left: Vector3D) (right: Vector3D) : Vector3D = {
    X = left.Y * right.Z - left.Z * right.Y
    Y = left.Z * right.X - left.X * right.Z
    Z = left.X * right.Y - left.Y * right.X
}

/// Computes the Euclidean length of a vector.
let vectorLength (vector: Vector3D) =
    sqrt (dotProduct vector vector)

/// Normalizes a vector to unit length when it is non-zero.
let normalizeVector (vector: Vector3D) : Result<Vector3D, string> =
    let length = vectorLength vector

    if length <= 1e-9 then
        Error "Cannot normalize a zero-length vector."
    else
        Ok (scaleVector (1.0 / length) vector)

/// Translates a point by a vector.
let translatePoint (point: Point3D) (offset: Vector3D) : Point3D = {
    X = point.X + offset.X
    Y = point.Y + offset.Y
    Z = point.Z + offset.Z
}

/// Creates a point from three scalar coordinates.
let createPoint x y z : Point3D = { X = x; Y = y; Z = z }

/// Creates a vector from three scalar coordinates.
let createVector x y z : Vector3D = { X = x; Y = y; Z = z }
