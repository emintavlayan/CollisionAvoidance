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


/// Finds the structure in the current structure set
let tryFindStructure (structureName : string) (structureSet : StructureSet) : Result<Structure, string> =
    structureSet.Structures
    |> Seq.tryFind (fun s -> s.Id.ToUpperInvariant() = structureName)
    |> function
        | Some structure ->
            Ok structure

        | None ->
            Error (structureName + " structure was not found.")


/// Gets all treatment beams from the current plan
let getTreatmentBeams (plan : PlanSetup) =
    plan.Beams
    |> Seq.filter (fun beam -> not beam.IsSetupField)
    |> Seq.toList


/// Creates the points representing the linac for the given treatment beams
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


/// Plotting function for help visualising element positioning
let plotting 
    (disk : VVector list) 
    (body : MeshGeometry3D) 
    (couch : MeshGeometry3D) 
    (hull : VVector list)
    (name : string)
    =
    let perimeter = disk |> List.tail

    // Helpers to split VVector list into x/y/z arrays
    let xs (pts: VVector list) = pts |> List.map (fun p -> p.x)

    let ys (pts: VVector list) = pts |> List.map (fun p -> p.y)

    let zs (pts: VVector list) = pts |> List.map (fun p -> p.z)

    let bodyx = [0 .. body.Positions.Count - 1] |> List.map(fun i -> body.Positions[i].X)
    let bodyy = [0 .. body.Positions.Count - 1] |> List.map(fun i -> body.Positions[i].Y)
    let bodyz = [0 .. body.Positions.Count - 1] |> List.map(fun i -> body.Positions[i].Z)
    let bodyi = [0 .. body.TriangleIndices.Count/3 - 1] |> List.map(fun i -> body.TriangleIndices[i * 3])
    let bodyj = [0 .. body.TriangleIndices.Count/3 - 1] |> List.map(fun i -> body.TriangleIndices[i * 3 + 1])
    let bodyk = [0 .. body.TriangleIndices.Count/3 - 1] |> List.map(fun i -> body.TriangleIndices[i * 3 + 2])
    let body3d = Chart.Mesh3D(x = bodyx, y = bodyy, z = bodyz, I = bodyi, J = bodyj, K = bodyk, Opacity = 1, Name = "BODY", Color = Color.fromKeyword Tan)
    
    let couchx = [0 .. couch.Positions.Count - 1] |> List.map(fun i -> couch.Positions[i].X)
    let couchy = [0 .. couch.Positions.Count - 1] |> List.map(fun i -> couch.Positions[i].Y)
    let couchz = [0 .. couch.Positions.Count - 1] |> List.map(fun i -> couch.Positions[i].Z)
    let couchi = [0 .. couch.TriangleIndices.Count/3 - 1] |> List.map(fun i -> couch.TriangleIndices[i * 3])
    let couchj = [0 .. couch.TriangleIndices.Count/3 - 1] |> List.map(fun i -> couch.TriangleIndices[i * 3 + 1])
    let couchk = [0 .. couch.TriangleIndices.Count/3 - 1] |> List.map(fun i -> couch.TriangleIndices[i * 3 + 2])
    let couch3d = Chart.Mesh3D(x = couchx, y = couchy, z = couchz, I = couchi, J = couchj, K = couchk, Opacity = 1, Name = "Couch", Color = Color.fromKeyword Magenta)
    
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
            LineColor = Color.fromKeyword Yellow,
            Name = "Hull"
        )

    // Combine and style
    [ diskTrace; body3d; couch3d; HullTrace]
    
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
    |> Chart.saveHtml ("//rghrhariafil/Radiofysik/Personlig/Nicklas/" + name)


/// Finds the structures of the given names. if any structure does not exist, returns an error
let findBodyStructures
    (structureSet : StructureSet)
    (structureNames : string[])
    : Result<Map<string,Structure>, string>
    =
    let structResults = 
        structureNames 
        |> Array.map(fun name -> tryFindStructure name structureSet)

    let anyError = Array.exists Result.isError structResults
    
    match anyError with 
    | false -> 
        let structureMap =
            structResults
            |> Array.map Result.toOption
            |> Array.zip structureNames 
            |> Array.filter(fun (name, structure) -> structure.IsSome)
            |> Array.map(fun (name, structure) -> (name, structure.Value))
            |> Map.ofArray
        
        Ok structureMap
    | true -> 
        let errorString = 
            structResults
            |> Array.indexed
            |> Array.where(fun (i, res) -> res.IsError)
            |> Array.map(fun (i, res) -> structureNames[i])
            |> Array.fold(fun acc err -> acc + " " + err + " structure was not found.") "Errors:"

        Error errorString


/// extracts the SnapshotVolume of all structures on the structure map
let findSnapshotVolumes
    (structureSet : StructureSet)
    (includeVacfix : bool)
    (structureMap : Map<string,Structure>)
    : Map<string,SnapshotVolume>
    =
    let volumeMap =
        structureMap
        |> Map.map(fun key structure ->  extractSnapshotVolume structureSet structure)

    let volumeMapvac =
            if includeVacfix && volumeMap.ContainsKey "BODY"  && volumeMap.ContainsKey "COUCHSURFACE" then
                let extraheight = 80.0<mm>
                let lm = 50.0<mm>
      
                let volumeVacfix = 
                    vacfixVolume
                        extraheight
                        lm
                        volumeMap.["BODY"]
                        volumeMap.["COUCHSURFACE"]
                volumeMap.Add("VACFIX", volumeVacfix) 
            else
                volumeMap
    volumeMapvac


/// Makes a hull of all the volumes i the volume map elements.
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

        let! mapOfStructures = findBodyStructures structureSet [|"BODY"; "COUCHSURFACE"|]

        let mapOfVolumes = findSnapshotVolumes structureSet true mapOfStructures

        let volume = makeConvexHullOfVolumes mapOfVolumes 
            
        
        let! bodyMesh =
            mapOfStructures.["BODY"].MeshGeometry
            |> BodyMeshSnapshot.create
        let! couchMesh =
            mapOfStructures.["COUCHSURFACE"].MeshGeometry
            |> BodyMeshSnapshot.create

        let diskPoints = 
            plan
            |> getTreatmentBeams
            |> createSliceAndDiskPointsFromBeams 550.0<mm> 20.0<mm> 390.0<mm>

        
        let bodyMeshValue = 
            bodyMesh
            |> BodyMeshSnapshot.value
        let couchMeshValue = 
            couchMesh
            |> BodyMeshSnapshot.value

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let filteredPoints = 
            diskPoints
            |> hasCollisionWithStructureParallelFilter volume
        stopWatch.Stop()

        showMessageBox ("All test took " + stopWatch.Elapsed.TotalMilliseconds.ToString() + " ms. for " + diskPoints.Length.ToString() + " points.")
        showMessageBox("Calculates " + (int((float diskPoints.Length)/stopWatch.Elapsed.TotalMilliseconds)).ToString() + " points pr second")

        let gaps = gapBCVolume mapOfVolumes.["BODY"] mapOfVolumes.["COUCHSURFACE"]
        
        let gapData = new System.IO.StreamWriter("//rghrhariafil/Radiofysik/Personlig/Nicklas/Test.csv")
        gaps |> Array.map(fun (z, g) -> string(z) + ";" + string(g) + "\n") |> Array.append [|"z;gap\n"|] |> String.concat(" ") |> gapData.Write
        gapData.Close()
        

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

        
        plotting diskPoints bodyMeshValue couchMeshValue VacfixLoop "test"
        
        if not filteredPoints.IsEmpty then
            plotting filteredPoints bodyMeshValue couchMeshValue ConvexHullLoops "filterTest"

        showMessageBox (diskPoints.Length.ToString() + " points generated")
        return!
            checkDiskPointsAgainstStructure volume diskPoints
    }

