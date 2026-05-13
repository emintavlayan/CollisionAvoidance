module VMS.TPS.Workflow

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open VMS.TPS.ContextRetrievalSafe
open VMS.TPS.DiskCreation
open VMS.TPS.PointInVolumeCheck
open VMS.TPS.DebugHelpers
open StructureSnapshot
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
        generateSlicesAndHalfDisksR beam offset resolution radius
        |> Array.collect id
        |> Array.toList)

// Checks if all control points are coplanar
let isBeamCoplanar 
    (beam : Beam)
    : bool
    =
    beam.ControlPoints
    |> Seq.forall(fun cp -> cp.PatientSupportAngle = 0.0)

// Checks if all beams in the plansetup is coplanar
let isPlanCoplanar
    (plan : PlanSetup)
    : bool 
    =
    plan.Beams
    |> Seq.filter(isBeamCoplanar)
    |> Seq.isEmpty




let plotting disk =
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
    let centerTrace =
        Chart.Scatter3D(
            x = [ diskCenter.x ],
            y = [ diskCenter.y ],
            z = [ diskCenter.z ],
            mode = Mode.Markers,
            Name = "Disk center"
        )

 
    // Combine and style
    [ diskTrace; centerTrace]
    |> Chart.combine
    |> Chart.withTitle "Disk on Beam Axis (radius 390 mm)"
    //|> Chart.withSize(1800,1000)
    |> Chart.withScene (
        Scene.init (
            XAxis = LinearAxis.init (Title = Title.init ("X (mm)")),
            YAxis = LinearAxis.init (Title = Title.init ("Y (mm)")),
            ZAxis = LinearAxis.init (Title = Title.init ("Z (mm)")),
            AspectMode = AspectMode.Data // equal aspect by data range
        )
    )
    |> Chart.show

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


        //let structureSetCopy : StructureSet = structureSet.Copy()

        
        let! body =
            tryFindBodyStructure structureSet

      
        let bodyMesh = body.MeshGeometry.Clone()
        

        let volume = extractSnapshotVolume structureSet body

        let diskPoints = 
            plan
            |> getTreatmentBeams
            |> createSliceAndDiskPointsFromBeams 550.0<mm> 5.0<mm> 390.0<mm>

        

        //plotting diskPoints


        showMessageBox (diskPoints.Length.ToString() + "points generated")
        return!
            //checkDiskPointsAgainstStructureMesh bodyMesh diskPoints
            checkDiskPointsAgainstStructure volume bodyMesh diskPoints
    } 