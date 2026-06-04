# Detailed Analysis And Visualization

Detailed analysis is the richer follow-up to the flat yes/no collision check.

## Current shape

- The server can build `DetailedCollisionResultDto` from detached DTO inputs.
- Detailed analysis preserves point provenance:
  - `beamId`
  - `controlPointIndex`
  - `gantryAngle`
  - `sampleType`
- The result keeps `collisionPoints` and can optionally keep `candidatePoints`.
- Beam-level and control-point-level groupings are included so later UI code does not need to rebuild them.

## Purpose

This result shape is the handoff to future 3D visualization and reporting.

It is intended to support later UI controls such as:

- beam checkboxes
- couch base toggle
- VacFix toggle
- breast board toggle
- show all points
- show only collision points
- show BODY
- show accessories

## What is still deferred

- Plotly rendering
- client-side collision pages
- accessory-derived provenance
- richer accessory models in the scene
- selective rerun controls from the UI
