# Contracts

`src-contracts/CollisionAvoidance.Contracts` is the shared language between the ESAPI exporter and the SAFE application.

## Boundary rules

- Contracts are ESAPI-free.
- Contracts do not depend on SAFE, WPF, Plotly, Saturn, Fable, or browser-specific APIs.
- Raw ESAPI objects never cross the boundary.
- Patient identifiers are obfuscated before a `CollisionRunRequestDto` is created for storage or transport.

## What the DTOs model

- Geometry DTOs describe points, vectors, bounds, meshes, and contour slices using only serializable F# records, lists, options, and simple unions.
- Plan DTOs intentionally resemble ESAPI concepts such as plans, beams, control points, gantry direction, and source position.
- Beam and control-point DTOs flatten awkward ESAPI access patterns by carrying clearly named beam-level values such as couch angle, gantry start/stop, collimator angle, isocenter, and source position.
- Structure DTOs model detached BODY and future accessory volumes through contour slices, bounds, and optional mesh data.
- Collision DTOs separate request data, flat yes/no analysis summaries, and detailed point-level provenance for later SAFE-side reporting and visualization.

## Why the contracts exist

- The ESAPI exporter can validate Eclipse context and extract detached data without leaking live ESAPI objects into the SAFE runtime.
- The SAFE server can evolve the analysis, storage, UI, and reporting model without depending on Varian runtime types.
- Both heads can change independently as long as the contracts remain stable and serializable.
