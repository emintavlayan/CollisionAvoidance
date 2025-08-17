// test_disk_creation.fsx

// ───────────────────────────────────────────────────────────────────────────────
// 1) Dependencies
// ───────────────────────────────────────────────────────────────────────────────
#r "nuget: Plotly.NET, 4.1.0"
#r "nuget: Plotly.NET.Interactive, 4.1.0"

open System
open System.Globalization
open Plotly.NET

// ───────────────────────────────────────────────────────────────────────────────
// 2) Load your codebase modules
// ───────────────────────────────────────────────────────────────────────────────

#load "../04_VectorMath.fs"
#load "../05_DiskCreation.fs"

// Bring your namespaces into scope (rename if your modules declare different namespaces)
open VMS.TPS.VectorMath
open VMS.TPS.DiskCreation

// ───────────────────────────────────────────────────────────────────────────────
// 3) Helpers: lightweight 3D point abstraction 
// ───────────────────────────────────────────────────────────────────────────────



// ───────────────────────────────────────────────────────────────────────────────
// 4) Call disk creation
// ───────────────────────────────────────────────────────────────────────────────


let diskResult =
    //...
    
// ───────────────────────────────────────────────────────────────────────────────
// 5) Visualization with Plotly.NET
// ───────────────────────────────────────────────────────────────────────────────

// Helper charts:
let chartPointCloud (pts: P3 list) =
    //...

let fig =
    //...

fig
