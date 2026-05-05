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
            plan
            |> getTreatmentBeams
            |> createDiskPointsFromBeams ... // not implemented yet 

        return!
            checkDiskPointsAgainstStructure body diskPoints
    }