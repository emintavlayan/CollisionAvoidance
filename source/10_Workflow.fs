module VMS.TPS.Workflow

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open VMS.TPS.ContextRetrievalSafe
open VMS.TPS.DiskCreation
open VMS.TPS.PointInVolumeCheck

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

        let! structureSet =
            tryGetCurrentStructureSet context

        let! body =
            tryFindBodyStructure structureSet


        let diskPoints = 
            if isPlanCoplanar plan then
                plan
                |> getTreatmentBeams
                |> createSliceAndDiskPointsFromBeams 1.0 550.0<mm> 20.0<mm> 390.0<mm>
                ///|> createDiskPointsFromBeams ... // not implemented yet 
            else
                plan
                |> getTreatmentBeams
                |> createSliceAndDiskPointsFromBeamsNCP 1.0 550.0<mm> 20.0<mm> 390.0<mm>
            

        return!
            checkDiskPointsAgainstStructure body diskPoints
    }