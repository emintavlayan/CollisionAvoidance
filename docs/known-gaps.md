# Known Gaps

- `src-Esapi` still uses compile-safe ESAPI-like adapter records in the normal build because local Varian ESAPI assemblies are not required for the SAFE solution.
- The current exporter mirrors the First-branch extraction behavior, but the final direct `ScriptContext` adapter layer is still to be wired in where local ESAPI references are available.
- BODY contour slices are required for first-version analysis, so mesh-only BODY exports are intentionally rejected by SAFE validation.
- Couch surface is optional, and VacFix, breast board, and couch base models are still work in progress.
- Detailed SAFE visualization remains future work; the current client only shows a minimal collision-run summary page.
