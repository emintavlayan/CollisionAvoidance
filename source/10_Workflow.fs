module VMS.TPS.Workflow

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open VMS.TPS.ContextRetrievalSafe
open VMS.TPS.DiskCreation
open VMS.TPS.PointInVolumeCheck
open VMS.TPS.DebugHelpers
open StructureSnapshot
open System.Windows.Media.Media3D;

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
    (arcStep : float)
    (offset : float<mm>)
    (resolution : float<mm>)
    (radius : float<mm>)
    (beams : Beam list)
    : VVector list
    =

    beams
    |> List.collect (fun beam ->
        generateSlicesAndHalfDisksR beam arcStep offset resolution radius
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

let createSliceAndDiskPointsFromBeamsNCP
    (arcStep : float)
    (offset : float<mm>)
    (resolution : float<mm>)
    (radius : float<mm>)
    (beams : Beam list)
    : VVector list
    =

    beams
    |> List.collect (fun beam ->
        generateSlicesAndHalfDisksRNCP beam arcStep offset resolution radius
        |> Array.collect id
        |> Array.toList)


/// Runs the current collision check workflow
let runCollisionCheckWorkflow
    (context : ScriptContext)
    : Result<string, string>
    =

    result {
        let! plan =
            tryGetCurrentPlan context

        showMessageBox "plan ok"

        let! structureSet =
            tryGetCurrentStructureSet context

        showMessageBox"structure ok"

        //let structureSetCopy : StructureSet = structureSet.Copy()


        let! body =
            tryFindBodyStructure structureSet

        showMessageBox "body ok"

        let bodyMesh = body.MeshGeometry.Clone()
        //let structureSetCopy = structureSet.Copy()

        //let! bodyCopy =
        //    tryFindBodyStructure structureSetCopy

        let volume = extractSnapshotVolume structureSet body

        let diskPoints = 
            plan
            |> getTreatmentBeams
            |> createSliceAndDiskPointsFromBeams 1 550.0<mm> 20.0<mm> 390.0<mm>
            (*
            if isPlanCoplanar plan then
                plan
                |> getTreatmentBeams
                |> createSliceAndDiskPointsFromBeams 5 550.0<mm> 10.0<mm> 390.0<mm>
            else
                plan
                |> getTreatmentBeams
                |> createSliceAndDiskPointsFromBeamsNCP 5 550.0<mm> 10.0<mm> 390.0<mm>
                *)
       
        
        
        showMessageBox (diskPoints.Length.ToString() + "points generated")
        return!
            //checkDiskPointsAgainstStructureMesh bodyMesh diskPoints
            checkDiskPointsAgainstStructureTest volume bodyMesh diskPoints
    } 