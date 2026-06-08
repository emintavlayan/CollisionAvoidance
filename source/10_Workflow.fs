module VMS.TPS.Workflow

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open FsToolkit.ErrorHandling
open VMS.TPS.ContextRetrievalSafe
open VMS.TPS.VectorMath
open VMS.TPS.DiskCreation
open VMS.TPS.PointInVolumeCheck
open VMS.TPS.DebugHelpers
open VMS.TPS.StructureSnapshot
open VMS.TPS.BodyMeshSnapshot
open VMS.TPS.VacfixInclusion
open System.Windows.Media.Media3D
open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.StyleParam
open System.Linq
open FSharp.Data


/// Finds the structure in the current structure set
let tryFindStructure (structureName : string) (structureSet : StructureSet) : Result<Structure, string> =
    structureSet.Structures
    |> Seq.tryFind (fun s -> s.Id.ToUpperInvariant() = structureName)
    |> function
        | Some structure ->
            Ok structure

        | None ->
            Error (structureName + " structure was not found.")

/// Temporary function for findBodyStructures
let tryFindStructure2 (structureName : string) (structureSet : StructureSet) : Structure option =
    let structure =
        structureSet.Structures
        |> Seq.tryFind (fun s -> s.Id.ToUpperInvariant() = structureName)
    if structure.IsNone then showMessageBox (structureName + " structure was not found.")
    structure
            

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


let plotting 
    (disk : VVector list) 
    (mesh : MeshGeometry3D) 
    (mesh2 : MeshGeometry3D) 
    (hull : VVector list)
    =
    let perimeter = disk |> List.tail

    // Helpers to split VVector list into x/y/z arrays
    let xs (pts: VVector list) = pts |> List.map (fun p -> p.x)

    let ys (pts: VVector list) = pts |> List.map (fun p -> p.y)

    let zs (pts: VVector list) = pts |> List.map (fun p -> p.z)

    let meshx = [0 .. mesh.Positions.Count - 1] |> List.map(fun i -> mesh.Positions[i].X)
    let meshy = [0 .. mesh.Positions.Count - 1] |> List.map(fun i -> mesh.Positions[i].Y)
    let meshz = [0 .. mesh.Positions.Count - 1] |> List.map(fun i -> mesh.Positions[i].Z)
    let meshi = [0 .. mesh.TriangleIndices.Count/3 - 1] |> List.map(fun i -> mesh.TriangleIndices[i * 3])
    let meshj = [0 .. mesh.TriangleIndices.Count/3 - 1] |> List.map(fun i -> mesh.TriangleIndices[i * 3 + 1])
    let meshk = [0 .. mesh.TriangleIndices.Count/3 - 1] |> List.map(fun i -> mesh.TriangleIndices[i * 3 + 2])
    let mesh3d = Chart.Mesh3D(x = meshx, y = meshy, z = meshz, I = meshi, J = meshj, K = meshk, Opacity = 1)
    
    let mesh2x = [0 .. mesh2.Positions.Count - 1] |> List.map(fun i -> mesh2.Positions[i].X)
    let mesh2y = [0 .. mesh2.Positions.Count - 1] |> List.map(fun i -> mesh2.Positions[i].Y)
    let mesh2z = [0 .. mesh2.Positions.Count - 1] |> List.map(fun i -> mesh2.Positions[i].Z)
    let mesh2i = [0 .. mesh2.TriangleIndices.Count/3 - 1] |> List.map(fun i -> mesh2.TriangleIndices[i * 3])
    let mesh2j = [0 .. mesh2.TriangleIndices.Count/3 - 1] |> List.map(fun i -> mesh2.TriangleIndices[i * 3 + 1])
    let mesh2k = [0 .. mesh2.TriangleIndices.Count/3 - 1] |> List.map(fun i -> mesh2.TriangleIndices[i * 3 + 2])
    let mesh23d = Chart.Mesh3D(x = mesh2x, y = mesh2y, z = mesh2z, I = mesh2i, J = mesh2j, K = mesh2k, Opacity = 1)
    
    // Traces: disk perimeter (line), disk center (marker), iso/src points (markers)
    let diskTrace =
        Chart.Scatter3D(
            x = xs perimeter,
            y = ys perimeter,
            z = zs perimeter,
            mode = Mode.Markers,
            Name = "Linac points"
        )

    let HullTrace =
        Chart.Scatter3D(
            x = xs hull,
            y = ys hull,
            z = zs hull,
            mode = Mode.Lines,
            Name = "Hull"
        )

    // Combine and style
    //[ diskTrace; mesh3d; mesh23d; HullTrace]
    [ diskTrace; mesh3d; mesh23d]
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

    //find proper way to save plot
    |> Chart.saveHtml "//rghrhariafil/Radiofysik/Personlig/Nicklas/test"
    //|> Chart.show



// WIP:
//errors in tryFindStructure replaced with just a warning
//if all are none return error
let findBodyStructures
    (structureSet : StructureSet)
    (includeVacfix : bool)
    (structureNames : string[])
    : Map<string,SnapshotVolume>
    =
    structureNames 
    |> Array.map(fun name -> tryFindStructure2 name structureSet)
    |> Array.zip structureNames 
    |> Array.filter(fun (name, structure) -> structure.IsSome)
    |> Array.map(fun (name, structure) -> (name, extractSnapshotVolume structureSet structure.Value))
    |> Map.ofArray
    |> fun volumeMap ->
        if includeVacfix && volumeMap.ContainsKey "BODY"  && volumeMap.ContainsKey "COUCHSURFACE" then
            let volumeVacfix = 
                vacfixVolume 
                    volumeMap.["BODY"]
                    volumeMap.["COUCHSURFACE"]

            volumeMap.Add ("VACFIX", volumeVacfix)
        else
            volumeMap
   

let makeConvexHullOfVolumes
    (volumeMap : Map<string,SnapshotVolume>)
    : SnapshotVolume
    =   
    volumeMap.Values.ToArray()
    |> fun volumes ->
        if volumeMap.Count > 1 then
            volumes
            |> Array.reduce (fun (hull : SnapshotVolume) vol -> findHullOfTwoVolumes hull vol)
        else 
            volumes
            |> Array.head 
            |> findHullOfVolume 

    
    


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
            tryFindStructure "BODY" structureSet

        let! couch =
            tryFindStructure "COUCHSURFACE" structureSet


        let mapOfVolumes = findBodyStructures structureSet true [|"BODY"; "COUCHSURFACE"|]
        let volume = makeConvexHullOfVolumes mapOfVolumes 
            
        

        let! bodyMesh =
            body.MeshGeometry
            |> BodyMeshSnapshot.create
        let! couchMesh =
            couch.MeshGeometry
            |> BodyMeshSnapshot.create

        let diskPoints = 
            plan
            |> getTreatmentBeams
            |> createSliceAndDiskPointsFromBeams 550.0<mm> 10.0<mm> 390.0<mm>

        
        //test filtering of points
        let bodyMeshValue = 
            bodyMesh
            |> BodyMeshSnapshot.value
        let couchMeshValue = 
            couchMesh
            |> BodyMeshSnapshot.value

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let filteredPoints = 
            diskPoints
            |> hasCollisionWithStructureParallelFilter volume bodyMeshValue
        stopWatch.Stop()

        showMessageBox ("All test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms. for " + diskPoints.Length.ToString() + " points.")
        showMessageBox("Calculates " + (int((float diskPoints.Length)/stopWatch.Elapsed.TotalMilliseconds)).ToString() + " points pr second")

        let gaps = gapBCVolume mapOfVolumes.["BODY"] mapOfVolumes.["COUCHSURFACE"]
        

        let ConvexHullLoops = 
            volume.slices
            |> Array.map(fun slice -> slice.loop)
            |> Array.concat
            |>Array.toList

        let VacfixLoop = 
            mapOfVolumes.["VACFIX"].slices
            |> Array.map(fun slice -> slice.loop)
            |> Array.concat
            |>Array.toList

        
        plotting diskPoints bodyMeshValue couchMeshValue ConvexHullLoops
            //plotting filteredPoints bodyMeshValue couchMeshValue ConvexHullLoops

        (*if not filteredPoints.IsEmpty then
            plotting filteredPoints bodyMeshValue couchMeshValue ConvexHullLoops*)

        showMessageBox (diskPoints.Length.ToString() + " points generated")
        return!
            bodyMesh
            |> BodyMeshSnapshot.value
            |> checkDiskPointsAgainstStructure volume diskPoints
    } 