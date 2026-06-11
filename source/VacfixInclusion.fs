module VMS.TPS.VacfixInclusion

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath
open VMS.TPS.StructureSnapshot
open FSharp.Stats
open FSharp.Stats.Fitting





//find couch maxy and body miny for a volume
let gapBCBB
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : float
    =
    abs (body.bounds.max.y - couch.bounds.min.y) 


//find couch maxy and body miny distance for a slice
let gapBCSlice
    (bodySlice : AxialSlice)
    (couchSlice : AxialSlice)
    : float
    =
    abs (bodySlice.bounds.maxY - couchSlice.bounds.minY) 
    

let gapBCVolume
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : (int*float*float) array
    =
    let zOverlapping =
        body.slices
        |> Array.map(fun p -> p.z)
        |> Array.filter (fun z -> 
            Array.contains z (couch.slices |> Array.map(fun p -> p.z)))
        
    let gap =
        zOverlapping
        |> Array.map(fun z ->
            Array.find (fun slice -> slice.z = z) body.slices,
            Array.find (fun slice -> slice.z = z) couch.slices)
        |> Array.map(fun (b, c) ->   (abs(b.bounds.maxY - c.bounds.minY)))

    let numbering = [|0 .. gap.Length - 1|]

    Array.zip zOverlapping gap
    |> Array.sortBy(fun (z, gap) -> z)
    |> Array.unzip
    ||> Array.zip3 numbering

//add an axialslice for vacfix
let vacfixSlice 
    (extraHeigth : float<mm>)
    (lm : float<mm>)
    (bodySlice : AxialSlice) 
    (couchSlice : AxialSlice)
    : AxialSlice
    =
    let gap = gapBCSlice bodySlice couchSlice
    
    let h = gap + float extraHeigth
    
    let vacfixSliceLoop = [|
        VVector(bodySlice.bounds.minX - float lm, couchSlice.bounds.minY, bodySlice.z);
        VVector(bodySlice.bounds.minX - float lm, couchSlice.bounds.minY - float h, bodySlice.z);
        VVector(bodySlice.bounds.maxX + float lm, couchSlice.bounds.minY - float h, bodySlice.z);
        VVector(bodySlice.bounds.maxX + float lm, couchSlice.bounds.minY, bodySlice.z)|]

    let vacfixSliceBounds = computeBoundingBox2D vacfixSliceLoop
    {
        z = bodySlice.z
        loop = vacfixSliceLoop
        bounds = vacfixSliceBounds
    }
    

//add a volume of vacfix
let vacfixVolume
    (extraHeigth : float<mm>)
    (lm : float<mm>)
    (body : SnapshotVolume)
    (couch : SnapshotVolume)
    : SnapshotVolume
    =
    let zOverlapping =
        body.slices
        |> Array.map(fun p -> p.z)
        |> Array.filter (fun z -> 
            Array.contains z (couch.slices |> Array.map(fun p -> p.z)))

    let vacficSlices =
        zOverlapping
        |> Array.map(fun z -> (
            Array.find (fun b -> b.z = z) body.slices,
            Array.find (fun c -> c.z = z) couch.slices ))
        |> Array.map(fun(b, c) -> vacfixSlice extraHeigth lm b c)

    let vacfixBounds = computeBoundingBox3D vacficSlices
    {
        slices = vacficSlices
        sliceThickness = body.sliceThickness
        bounds = vacfixBounds
    }


let findBBSlice
    (z : float)
    (xmin : float)
    (xmax : float)
    (ymax : float)
    (ymin : float)
    : AxialSlice
    =
    let BBSliceLoop = [|
        VVector(xmin, ymin, z);
        VVector(xmin, ymax, z);
        VVector(xmax, ymax, z);
        VVector(xmax, ymin, z)|]

    let bounds = computeBoundingBox2D BBSliceLoop
    {
        z = z
        loop = BBSliceLoop
        bounds =  bounds
    }


let findBreastBoard
    (body : SnapshotVolume)
    (couch : SnapshotVolume)
    : SnapshotVolume
    =
    let zBBmin = body.bounds.min[2] - 100.
    let zBBmax = body.bounds.max[2] + 100. 
    let zPoints = [zBBmin .. body.sliceThickness .. zBBmax]

    let (index, z, gap) = 
        gapBCVolume body couch
        |> Array.unzip3

    let fittingCoef = LinearRegression.fit(vector z, vector (Array.map(fun g -> couch.bounds.min[1] - g) gap),Method.SimpleLinear)

    let boardY =
        zPoints
        |> List.map(fun x -> LinearRegression.predict fittingCoef x)
        |> List.map(fun y ->
            if y < body.bounds.min[1] then
                body.bounds.min[1]
            elif y > body.bounds.max[1] then
                body.bounds.max[1]
            else
                y
        )

    
    let slicesBB =
        (zPoints, boardY)
        ||> List.map2(fun z maxy -> findBBSlice z couch.bounds.min[0] couch.bounds.max[0] couch.bounds.min[1] maxy)
        |> List.toArray

    let boundsBB = computeBoundingBox3D slicesBB
    
    {
        slices = slicesBB
        sliceThickness = body.sliceThickness
        bounds = boundsBB
    }

