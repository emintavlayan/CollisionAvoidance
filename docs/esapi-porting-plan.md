# ESAPI Porting Plan

## ESAPI extraction

- main entry point
- validation
- BODY finding
- MeshGeometry3D Clone and Freeze
- contour slice extraction
- beam extraction
- control point extraction
- source position extraction
- DTO building
- patient id obfuscation
- HTTP POST to SAFE server

## Ported From First-branch

- validation
- BODY lookup
- optional couch lookup
- BODY contour extraction
- mesh clone/freeze
- beam/control-point extraction
- source-position extraction
- request creation
- JSON fallback
- SAFE submission

## Collision logic to port later into SAFE server

- flat disk point creation
- half-disk/cap creation
- bounding-box filtering
- point-in-contour volume check
- PSeq behavior if still needed server-side
- fast yes/no result
- deeper analysis with provenance:
- beam id
- control point index
- gantry angle
- point coordinates
- point sample type
- collect collision points for 3D visualization

## Future SAFE analysis

- add couch base model
- add VacFix model
- add breast board model
- beam checkboxes
- accessory toggles
- Plotly 3D view
- detailed report
