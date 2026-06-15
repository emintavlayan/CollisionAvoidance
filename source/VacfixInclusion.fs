module VMS.TPS.VacfixInclusion

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath
open VMS.TPS.StructureSnapshot
open FSharp.Stats
open FSharp.Stats.Fitting





/// Finds gap between the bounding boxes of body and couch
let gapBCBB
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : float
    =
    abs (body.bounds.max.y - couch.bounds.min.y) 


/// Finds the gap between the AxialSlice of body and couch
let gapBCSlice
    (bodySlice : AxialSlice)
    (couchSlice : AxialSlice)
    : float
    =
    abs (bodySlice.bounds.maxY - couchSlice.bounds.minY) 
    
/// Returns an array containing the z value and gap for all AxialSclices in body and couch
let gapBCVolume
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : (float*float) array
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

    Array.zip zOverlapping gap
    |> Array.sortBy(fun (z, gap) -> z)


/// Generates an axialslice of vacfix for given body and couch
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
    

/// Generates the volume of vacfix
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

/// Generates a rectangular Axialslice from the given parameters
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

/// Generated the breastboard between the body and couch by fitting to the gap between the two
let findBreastBoard
    (body : SnapshotVolume)
    (couch : SnapshotVolume)
    : SnapshotVolume
    =
    let zBBmin = body.bounds.min[2] - 100.
    let zBBmax = body.bounds.max[2] + 100. 
    let zPoints = [zBBmin .. body.sliceThickness .. zBBmax]

    let (z, gap) = 
        gapBCVolume body couch
        |> Array.unzip

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

