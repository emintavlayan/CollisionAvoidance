module VMS.TPS.Workflow

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open VMS.TPS.ContextRetrievalSafe
open VMS.TPS.DiskCreation
open VMS.TPS.PointInVolumeCheck
open VMS.TPS.DebugHelpers
open VMS.TPS.StructureSnapshot
open VMS.TPS.BodyMeshSnapshot
open System.Windows.Media.Media3D
open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.StyleParam


/// Finds the BODY structure in the current structure set
let tryFindBodyStructure (structureSet : StructureSet) : Result<Structure, string> =
    structureSet.Structures
    |> Seq.tryFind (fun s -> s.Id.ToUpperInvariant() = "BODY")
    |> function
        | Some body ->
            Ok body

        | None ->
            Error "BODY structure was not found."

/// Gets all treatment beams from the current plan
let getTreatmentBeams (plan : PlanSetup) =
    plan.Beams
    |> Seq.filter (fun beam -> not beam.IsSetupField)
    |> Seq.toList

/// Creates a flat list of disk points from all treatment beams
let createDiskPointsFromBeams
    (arcStep : float)
    (offset : float<mm>)
    (pointsPerDisk : int)
    (beams : Beam list)
    : VVector list
    =

    beams
    |> List.collect (fun beam ->
        generateDisksForBeam beam arcStep offset pointsPerDisk
        |> Array.collect id
        |> Array.toList)

let createSliceAndDiskPointsFromBeams
    //(arcStep : float)
    (offset : float<mm>)
    (resolution : float<mm>)
    (radius : float<mm>)
    (beams : Beam list)
    : VVector list
    =

    beams
    |> List.collect (fun beam ->
        generateSlicesAndHalfDisks beam offset resolution radius
        |> Array.collect id
        |> Array.toList)


let plotting (disk : VVector list) (mesh : MeshGeometry3D) =
    let perimeter = disk |> List.tail

    // Helpers to split VVector list into x/y/z arrays
    let xs (pts: VVector list) = pts |> List.map (fun p -> p.x)

    let ys (pts: VVector list) = pts |> List.map (fun p -> p.y)

    let zs (pts: VVector list) = pts |> List.map (fun p -> p.z)

    let meshx = 
        [0 .. mesh.Positions.Count - 1]
        |> List.map(fun i -> mesh.Positions[i].X)
    let meshy = 
        [0 .. mesh.Positions.Count - 1]
        |> List.map(fun i -> mesh.Positions[i].Y)
    let meshz = 
        [0 .. mesh.Positions.Count - 1]
        |> List.map(fun i -> mesh.Positions[i].Z)


    let meshi = 
        [0 .. mesh.TriangleIndices.Count/3 - 1]
        |> List.map(fun i -> mesh.TriangleIndices[i * 3])
    let meshj = 
        [0 .. mesh.TriangleIndices.Count/3 - 1]
        |> List.map(fun i -> mesh.TriangleIndices[i * 3 + 1])
    let meshk = 
        [0 .. mesh.TriangleIndices.Count/3 - 1]
        |> List.map(fun i -> mesh.TriangleIndices[i * 3 + 2])

    let mesh3d = Chart.Mesh3D(x = meshx, y = meshy, z = meshz, I = meshi, J = meshj, K = meshk, Opacity = 0.5)
    

    // Traces: disk perimeter (line), disk center (marker), iso/src points (markers)
    let diskTrace =
        Chart.Scatter3D(
            x = xs perimeter,
            y = ys perimeter,
            z = zs perimeter,
            mode = Mode.Markers,
            Name = "Linac points"
        )

    // Combine and style
    [ diskTrace; mesh3d]
    |> Chart.combine
    |> Chart.withTitle "Test"
    |> Chart.withSize(1800,1000)
    |> Chart.withScene (
        Scene.init (
            XAxis = LinearAxis.init (Title = Title.init ("X (mm)")),
            YAxis = LinearAxis.init (Title = Title.init ("Y (mm)")),
            ZAxis = LinearAxis.init (Title = Title.init ("Z (mm)")),
            AspectMode = AspectMode.Data // equal aspect by data range
        )
    )
    |> Chart.saveHtml "//rghrhariafil/Radiofysik/Personlig/Nicklas/test"
    //|> Chart.show
    

/// Runs the current collision check workflow
let runCollisionCheckWorkflow
    (context : ScriptContext)
    : Result<string, string>
    =

    result {
        let! plan =
            tryGetCurrentPlan context

        let! structureSet =
            tryGetCurrentStructureSet context
        
        let! body =
            tryFindBodyStructure structureSet

        let volume = extractSnapshotVolume structureSet body
        
        let! bodyMesh =
            body.MeshGeometry
            |> BodyMeshSnapshot.create

        let diskPoints = 
            plan
            |> getTreatmentBeams
            |> createSliceAndDiskPointsFromBeams 550.0<mm> 5.0<mm> 390.0<mm>

        //test filtering of points
        let test = 
            bodyMesh
            |> BodyMeshSnapshot.value
        let filteredPoints = 
            diskPoints
            |> hasCollisionWithStructureParallelFilter volume test

        plotting filteredPoints test 

        showMessageBox (diskPoints.Length.ToString() + " points generated")
        return!
            bodyMesh
            |> BodyMeshSnapshot.value
            |> checkDiskPointsAgainstStructure volume diskPoints
    } 