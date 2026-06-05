# ESAPI Exporter

`src-Esapi/CollisionAvoidance.EsapiExporter` is the clinical data extraction boundary for Eclipse-side execution.

## Responsibilities

- Validate the current Eclipse context before extraction begins.
- Require BODY and accept an optional couch-surface structure.
- Build detached DTOs that match `src-contracts`.
- Obfuscate the patient id before any request leaves the Eclipse-side process.
- Send, or later send, the detached request to the SAFE server.
- Optionally write a local JSON fallback when server submission is not ready.

## Non-responsibilities

- The ESAPI exporter should not own detailed collision analysis.
- The ESAPI exporter should not own plotting or rich visualization.
- The ESAPI exporter should not leak live ESAPI, WPF, or Eclipse runtime objects into SAFE.

## Current shape

- The project currently uses compile-safe ESAPI-like input records so the extraction boundary can build without local Varian DLL references.
- The exporter validates the full context in one pass, treating BODY as required and couch surface as optional.
- The exported request contains detached BODY contour slices, optional BODY mesh data, plan, beam, and control-point DTOs, plus an optional couch-surface accessory DTO.
- Patient id obfuscation happens before the request is written to JSON or sent to SAFE.
- Raw ESAPI objects never cross the exporter boundary.
- The module names and DTO mapping steps follow the mental model from the student prototype: validate context, extract body/beam/control-point data, obfuscate patient identity, and submit detached DTOs.
- When real ESAPI references are wired in later, the current `Like` inputs can be replaced by direct Eclipse adapters without changing the contract DTO layer.

## Ported From First-branch

- Validation follows the safe context retrieval pattern from `source/02_ContextRetrievalSafe.fs`.
- BODY lookup remains a case-insensitive structure-id lookup for `BODY`.
- Couch surface lookup is optional and uses detached structure-name heuristics.
- BODY contour extraction remains the first-version volume source, mirroring `source/08_StructureSnapshot.fs`.
- Mesh extraction preserves the clone-and-freeze boundary from `source/BodyMeshSnapshot.fs`, but only detached mesh DTOs leave the exporter.
- Beam and control-point extraction preserve the prototype beam model: treatment-beam filtering, gantry start/stop from control points, exposed couch angle, collimator angle, isocenter, and source position.
- Request creation preserves the detached workflow from the prototype, but now targets contract DTOs instead of live runtime objects.
- Local JSON fallback is always written before SAFE submission so exporter output exists even if the server is unavailable.
- SAFE submission stays outside the DTO mapping layer and only posts detached JSON.
