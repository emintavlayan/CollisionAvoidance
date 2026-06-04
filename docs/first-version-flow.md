# First Version Flow

The first runnable clinical flow is:

1. ESAPI validates the current patient, course, plan, structure set, BODY, and treatment beams.
2. ESAPI builds a detached `CollisionRunRequestDto`.
3. ESAPI obfuscates the patient id before writing JSON or sending the request.
4. SAFE creates a collision run and executes the default flat BODY analysis immediately.
5. SAFE exposes the stored result at `/api/collision-runs/{runId}` and `/api/collision-runs/{runId}/summary`.
6. The client route `/collision/{runId}` shows a minimal run summary.

## Current first-version scope

- BODY is required.
- Couch surface is optional.
- Accessories beyond couch surface are work in progress.
- Plotly and rich 3D visualization are not implemented yet.
- A server-only local run serves a placeholder page at `/collision/{runId}` until the client bundle is published or the client dev server is running.
- The UI currently focuses on summary data only:
  - run id
  - obfuscated patient id
  - plan id
  - status
  - generated point count
  - candidate point count
  - inside point count
  - beam ids
