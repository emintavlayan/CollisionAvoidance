# Collision Analysis

The current SAFE-side collision analysis runs entirely on detached DTOs from `src-contracts`.

## First runnable scope

- BODY is required.
- Couch surface is optional.
- The current default run path remains safe when no couch structure is supplied.
- VacFix, breast board, and couch base remain work in progress.

## Current algorithm stages

1. Beam/control-point sampling:
   - detached beam snapshots provide source positions and isocenters
   - control-point data is preferred when available
2. Clearance point generation:
   - line samples are generated along the beam-aligned clearance path
   - first and last half-disk caps are generated to preserve the prototype sampling shape
3. Bounds filtering:
   - candidate points are filtered against the BODY 3D bounds first
4. Contour-slice volume checking:
   - the BODY slice is selected by axial `Z`
   - a 2D point-in-polygon test is applied in the slice plane
5. Result building:
   - flat analysis returns a fast yes/no summary with counts
   - detailed analysis returns collision points with provenance

## What was ported from First-branch

- clearance point generation along the beam axis
- first/last half-disk cap generation
- BODY bounds filtering before contour checks
- axial slice lookup
- 2D ray-casting polygon containment
- fast collision status and detailed collision-point collection

## What is intentionally deferred

- Plotly or other visualization output
- full mesh point-in-mesh checks
- PSeq-based parallel execution
- final couch base, VacFix, and breast-board volume models
- clinically complete accessory scene composition beyond BODY and optional couch support
