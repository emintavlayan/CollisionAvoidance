// testDiskCreation.fsx

// --- deps ---
#r "nuget: Plotly.NET, 4.1.0"
#r "nuget: Plotly.NET.Interactive, 4.1.0"
#r "nuget: Plotly.NET, 4.2.0"
#r "nuget: Plotly.NET.CSharp, 0.7.0" // brings StyleParam conveniences (optional)
#r "../esapi18/VMS.TPS.Common.Model.API.dll"
#r "../esapi18/VMS.TPS.Common.Model.Types.dll"

open System
open System.Globalization
open Plotly.NET

#load "../src/03_BeamGeometry.fs"
#load "../src/04_VectorMath.fs"
#load "../src/05_DiskCreation.fs"


open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.StyleParam

open VMS.TPS.Common.Model.Types
open VMS.TPS.DiskCreation // expose generateDiskOnBeamAxis

// Define the two points as VVectors
let iso = VVector(0.0, 0.0, 0.0) // (0,0,0)

let src = VVector(0.0, 1000.0, 0.0) // (0,100,0)

// Create a disk on the beam axis. Offset 0 mm -> disk centered at isocenter.
// pointsPerDisk controls the circle resolution.
let pointsPerDisk = 24 // Number of points on the disk perimeter

let disk = generateDiskOnBeamAxis iso src 900.0<mm> pointsPerDisk

let diskCenter = List.head disk

let perimeter = disk |> List.tail

// Helpers to split VVector list into x/y/z arrays
let xs (pts: VVector list) = pts |> List.map (fun p -> p.x)

let ys (pts: VVector list) = pts |> List.map (fun p -> p.y)

let zs (pts: VVector list) = pts |> List.map (fun p -> p.z)

// Close the perimeter loop for nicer plotting
let perimeterClosed =
    match perimeter with
    | [] -> []
    | ps -> ps @ [ List.head ps ]

// Traces: disk perimeter (line), disk center (marker), iso/src points (markers)
let diskTrace =
    Chart.Scatter3D(
        x = xs perimeterClosed,
        y = ys perimeterClosed,
        z = zs perimeterClosed,
        mode = Mode.Lines,
        Name = "Disk perimeter"
    )

let centerTrace =
    Chart.Scatter3D(
        x = [ diskCenter.x ],
        y = [ diskCenter.y ],
        z = [ diskCenter.z ],
        mode = Mode.Markers,
        Name = "Disk center"
    )

let isoTrace =
    Chart.Scatter3D(x = [ iso.x ], y = [ iso.y ], z = [ iso.z ], mode = Mode.Markers, Name = "ISO (0,0,0)")

let srcTrace =
    Chart.Scatter3D(x = [ src.x ], y = [ src.y ], z = [ src.z ], mode = Mode.Markers, Name = "Source (0,100,0)")

// Combine and style
[ diskTrace; centerTrace; isoTrace; srcTrace ]
|> Chart.combine
|> Chart.withTitle "Disk on Beam Axis (radius 390 mm)"
|> Chart.withScene (
    Scene.init (
        XAxis = LinearAxis.init (Title = Title.init ("X (mm)")),
        YAxis = LinearAxis.init (Title = Title.init ("Y (mm)")),
        ZAxis = LinearAxis.init (Title = Title.init ("Z (mm)")),
        AspectMode = AspectMode.Data // equal aspect by data range
    )
)
|> Chart.show
