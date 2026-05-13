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

#load "../source/03_BeamGeometry.fs"
#load "../source/04_VectorMath.fs"
#load "../source/05_DiskCreation.fs"
#load "../source/08_StructureSnapshot.fs"
//#load "../source/09_PointInVolumeCheck.fs"

open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.StyleParam

open VMS.TPS.Common.Model.Types
open VMS.TPS.DiskCreation // expose generateDiskOnBeamAxis
open VMS.TPS.StructureSnapshot
open VMS.TPS.VectorMath
//open VMS.TPS.PointInVolumeCheck



let angle = System.Math.PI/2.*3.
let shift = VVector(-200.0, -150.0, -740.0)
let beampositions = 
    [0.0 .. 0.1 .. Math.PI]
    |> List.map(fun theta -> (VVector(0.0, 0.0, 0.0), VVector(1000.0*Math.Sin(theta)*System.Double.Cos(angle), 1000.0*Math.Cos(theta), -1000.0*Math.Sin(theta)*System.Double.Sin(angle))))
    |> List.toArray
    |> Array.map(fun (iso, src) -> vadd iso shift, vadd src shift)
    
let iso, src = beampositions[0]

//volume
let simpleBoxLen = 100.0
let offsetX = 550.0
let offsetY = 0.0
let offsetZ = 0.0
let boundingBox2D : BoundingBox2D = {
    minX = -simpleBoxLen/2.0+offsetX
    maxX = simpleBoxLen/2.0+offsetX
    minY = -simpleBoxLen/2.0+offsetY
    maxY = simpleBoxLen/2.0+offsetY
}
let min3D = VVector(-simpleBoxLen/2.0+offsetX, -simpleBoxLen/2.0+offsetY, -simpleBoxLen/2.0+offsetZ)
let max3D = VVector(simpleBoxLen/2.0+offsetX, simpleBoxLen/2.0+offsetY, simpleBoxLen/2.0+offsetZ)
let boundingBox3D : BoundingBox3D = {
    min = min3D
    max = max3D}
let point1 = VVector(simpleBoxLen/2.0+offsetX, simpleBoxLen/2.0+offsetY, simpleBoxLen/2.0+offsetZ)
let point2 = VVector(-simpleBoxLen/2.0+offsetX, simpleBoxLen/2.0+offsetY, simpleBoxLen/2.0+offsetZ)
let point3 = VVector(-simpleBoxLen/2.0+offsetX, -simpleBoxLen/2.0+offsetY, simpleBoxLen/2.0+offsetZ)
let point4 = VVector(simpleBoxLen/2.0+offsetX, -simpleBoxLen/2.0+offsetY, simpleBoxLen/2.0+offsetZ)
let point5 = VVector(simpleBoxLen/2.0+offsetX, simpleBoxLen/2.0+offsetY, -simpleBoxLen/2.0+offsetZ)
let point6 = VVector(-simpleBoxLen/2.0+offsetX, simpleBoxLen/2.0+offsetY, -simpleBoxLen/2.0+offsetZ)
let point7 = VVector(-simpleBoxLen/2.0+offsetX, -simpleBoxLen/2.0+offsetY, -simpleBoxLen/2.0+offsetZ)
let point8 = VVector(simpleBoxLen/2.0+offsetX, -simpleBoxLen/2.0+offsetY, -simpleBoxLen/2.0+offsetZ)
let loop1 = [point1; point2; point3; point4]
let loop2 = [point5; point6; point7; point8]
let slice1 : AxialSlice = {
    z = simpleBoxLen/2.0+offsetZ
    loop = List.toArray loop1
    bounds = boundingBox2D}
let slice2 : AxialSlice = {
    z = -simpleBoxLen/2.0+offsetZ
    loop = List.toArray loop2
    bounds = boundingBox2D}
let slices = [|slice1; slice2|]
let volume : SnapshotVolume = {
    slices = slices
    sliceThickness = 20.0
    bounds = boundingBox3D}

let combinedoPoints = [point1; point2; point3; point4; point5; point6; point7; point8]

// Create a disk on the beam axis. Offset 0 mm -> disk centered at isocenter.
// pointsPerDisk controls the circle resolution.
let pointsPerDisk = 100 // Number of points on the disk perimeter
let pointsPerLine = 100
let radius = 390.0<mm>
let stopwatch = System.Diagnostics.Stopwatch.StartNew()
//let disk = generateSlicesAndHalfDisksModified beampositions 550.0<mm> pointsPerDisk pointsPerLine radius 20.0<mm> 
let disk = generateSlicesAndHalfDisksRModified beampositions 550.0<mm> 50.0<mm> radius angle

let disks = List.toArray disk

printfn "Number of points: %i"(List.length(disk))
stopwatch.Stop()
printfn "runtime: %f ms" stopwatch.Elapsed.TotalMilliseconds
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
        mode = Mode.Markers,
        Name = "Disk perimeter"
    )
let boxTrace =
    Chart.Scatter3D(
        x = xs combinedoPoints,
        y = ys combinedoPoints,
        z = zs combinedoPoints,
        mode = Mode.Lines,
        Name = "Box perimeter"
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
[ diskTrace; centerTrace; isoTrace; srcTrace; boxTrace]
|> Chart.combine
|> Chart.withTitle "Disk on Beam Axis (radius 390 mm)"
|> Chart.withSize(1800,1000)
|> Chart.withScene (
    Scene.init (
        XAxis = LinearAxis.init (Title = Title.init ("X (mm)")),
        YAxis = LinearAxis.init (Title = Title.init ("Y (mm)")),
        ZAxis = LinearAxis.init (Title = Title.init ("Z (mm)")),
        AspectMode = AspectMode.Data // equal aspect by data range
    )
)
|> Chart.show

