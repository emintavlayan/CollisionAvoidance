module VMS.TPS.VacfixInclusion

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath
open VMS.TPS.StructureSnapshot
open FSharp.Stats
open FSharp.Stats.Fitting





//find couch maxy and body niny for a volume
let gapBCBB
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : float<mm>
    =
    mmConv (abs (body.bounds.min.y - couch.bounds.max.y)) 


//find couch maxy and body miny distance for a slice
let gapBCSlice
    (bodySlice : AxialSlice)
    (couchSlice : AxialSlice)
    : float<mm>
    =
    mmConv (abs (bodySlice.bounds.maxY - couchSlice.bounds.minY)) 
    

let gapBCVolume
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : (int*float<mm>*float<mm>) array
    =
    let zOverlapping =
        body.slices
        |> Array.map(fun p -> mmConv p.z)
        |> Array.filter (fun z -> 
            Array.contains z (couch.slices |> Array.map(fun p -> mmConv p.z)))
        
    let gap =
        zOverlapping
        |> Array.map(fun z ->
            Array.find (fun slice -> mmConv slice.z = z) body.slices,
            Array.find (fun slice -> mmConv slice.z = z) couch.slices)
        |> Array.map(fun (b, c) ->  mmConv (abs( b.bounds.maxY - c.bounds.minY)))

    let numbering = [|0 .. gap.Length - 1|]

    Array.zip zOverlapping gap
    |> Array.sortBy(fun (z, gap) -> z)
    |> Array.unzip
    ||> Array.zip3 numbering

//add an axialslice for vacfix
let vacfixSlice 
    (bodySlice : AxialSlice) 
    (couchSlice : AxialSlice)
    : AxialSlice
    =
    let gap = gapBCSlice bodySlice couchSlice
    let extraHeigth = 80.<mm>
    
    let h = gap + extraHeigth
    let lm = 50.<mm>
    
    let vacfixSliceLoop = [|
        VVector(bodySlice.bounds.minX - mmFrom lm, couchSlice.bounds.minY, bodySlice.z);
        VVector(bodySlice.bounds.minX - mmFrom lm, couchSlice.bounds.minY - mmFrom h, bodySlice.z);
        VVector(bodySlice.bounds.maxX + mmFrom lm, couchSlice.bounds.minY - mmFrom h, bodySlice.z);
        VVector(bodySlice.bounds.maxX + mmFrom lm, couchSlice.bounds.minY, bodySlice.z)|]

    let vacfixSliceBounds = computeBoundingBox2D vacfixSliceLoop
    {
        z = bodySlice.z
        loop = vacfixSliceLoop
        bounds = vacfixSliceBounds
    }


//add a volume of vacfix
let vacfixVolume
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
        |> Array.map(fun(b, c) -> vacfixSlice b c)

    let vacfixBounds = computeBoundingBox3D vacficSlices
    {
        slices = vacficSlices
        sliceThickness = body.sliceThickness
        bounds = vacfixBounds
    }

//include breast board:

//make a fit for the gap length
//make a triangle based on the fit from zmin to zmax
// set the y minimum value of the board bodyymin and maximum bodyymax
let findBBSlice
    (z : float)
    (xmin : float)
    (xmax : float)
    (ymin : float)
    (ymax : float)
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
    let zBBmin = body.bounds.min[2]
    let zBBmax = body.bounds.max[2]
    let zPoints = [zBBmin .. body.sliceThickness .. zBBmax]

    let (index, z, gap) = 
        gapBCVolume body couch
        |> Array.unzip3

    let fittingCoef = LinearRegression.fit(vector (Array.map mmFrom z ), vector (Array.map mmFrom gap),Method.SimpleLinear)
    let fittedY = 
        zPoints
        |> List.map(fun x -> LinearRegression.predict(fittingCoef) x)

    let boardY =
        fittedY
        |> List.map(fun y ->
            if y < body.bounds.min[1] then
                body.bounds.min[1]
            elif y < body.bounds.max[1] then
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

