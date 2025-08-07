(**
    Module: VectorMath
    Purpose: 
        - Provides mathematical helper functions for working with VVector instances,
        - including basic arithmetic, scaling, and geometric operations.

    Why:
        - Encapsulates common vector math in one place,
        - avoids repeating low-level calculations in scripts,
        - improves readability and reduces risk of mathematical errors.

    Notes:
        - Operates on VMS.TPS.Common.Model.Types.VVector,
        - all functions are inline for performance,
        - follows conventional vector math definitions (right-hand rule for cross product).
*)

module VMS.TPS.VectorMath

open VMS.TPS.Common.Model.Types

/// Adds two VVector instances component-wise.
let inline vadd (a : VVector) (b : VVector) =
    a + b

/// Subtracts the second VVector from the first component-wise.
let inline vsub (a : VVector) (b : VVector) =
    a - b

/// Scales a VVector by a scalar value.
let inline vscale (a : VVector) (s : float) =
    a * s

/// Computes the dot (scalar) product of two VVectors.
let inline vdot (a : VVector) (b : VVector) =
    a.ScalarProduct(b)

/// Returns the length (magnitude) of a VVector.
let inline vlen (a : VVector) =
    a.Length

/// Returns the unit vector (normalized vector) of a VVector.
let inline vnormalize (a : VVector) =
    a.GetUnitLengthScaledVector()

/// Computes the cross product of two VVectors.
let inline vcross (a : VVector) (b : VVector) =
    let x =
        a.y * b.z - a.z * b.y

    let y =
        a.z * b.x - a.x * b.z

    let z =
        a.x * b.y - a.y * b.x

    VVector(x, y, z)

/// Calculates the distance between two points represented by VVectors.
let inline vdist (a : VVector) (b : VVector) : float =
    VVector.Distance(a, b)

/// Checks if vector is undefined (contains NaN or Infinity)
let inline visUndefined (v : VVector) : bool =
    v.IsUndefined()

/// Performs epsilon-equality check between vectors.
let inline vequal (a : VVector) (b : VVector) (epsilon : float) : bool =
    a.EpsilonEqual(b, epsilon)
