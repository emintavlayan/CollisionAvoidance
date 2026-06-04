# ESAPI Exporter

`src-Esapi/CollisionAvoidance.EsapiExporter` is the clinical data extraction boundary for Eclipse-side execution.

## Responsibilities

- Validate the current Eclipse context before extraction begins.
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
- The module names and DTO mapping steps follow the mental model from the student prototype: validate context, extract body/beam/control-point data, obfuscate patient identity, and submit detached DTOs.
- When real ESAPI references are wired in later, the current `Like` inputs can be replaced by direct Eclipse adapters without changing the contract DTO layer.
