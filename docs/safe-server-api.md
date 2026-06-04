# SAFE Server API

The SAFE server now exposes a minimal collision-run boundary in addition to the existing todo sample.

## Endpoints

- `POST /api/collision-runs`
  - Accepts a detached `CollisionRunRequestDto`
  - Requires a valid BODY snapshot
  - Accepts a missing couch surface
  - Creates a `Guid` run id
  - Executes the default flat collision analysis immediately
  - Stores the original request and summary in memory
  - Returns `CreateCollisionRunResponseDto`

- `GET /api/collision-runs/{runId}/summary`
  - Returns the stored `CollisionRunSummaryDto`

- `GET /api/collision-runs/{runId}`
  - Returns `CollisionRunDetailsDto`
  - Includes both the detached request payload and the stored summary

## Flow

The intended ESAPI flow is:

1. ESAPI extracts detached DTO data.
2. ESAPI posts `CollisionRunRequestDto` to SAFE.
3. SAFE returns the new `runId`.
4. ESAPI opens `/collision/{runId}` later when the dedicated collision UI exists.

## Current constraints

- Storage is in-memory only.
- The create endpoint runs the flat BODY analysis by default.
- Invalid BODY payloads return `400`.
- Couch surface is optional in the first runnable version.
- Detailed analysis is not triggered automatically yet.
- The todo SAFE sample remains intact so the template app still runs.
