module VMS.TPS.VacfixInclusion

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.DebugHelpers
open VMS.TPS.VectorMath
open VMS.TPS.StructureSnapshot


//find couch maxy and body niny for a volume
let distanceBCBB
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : float 
    =
    abs (body.bounds.min.y - couch.bounds.max.y)


//find couch maxy and body miny distance for a slice
let distanceBCSlice
    (bodySlice : AxialSlice)
    (couchSlice : AxialSlice)
    : float
    =
    abs (bodySlice.bounds.maxY - couchSlice.bounds.minY)

let didistanceBCVolume
    (body : SnapshotVolume) 
    (couch : SnapshotVolume) 
    : (float*float) array
    =
    let zOverlapping =
        body.slices
        |> Array.map(fun p -> p.z)
        |> Array.filter (fun z -> 
            Array.contains z (couch.slices |> Array.map(fun p -> p.z)))
        
    let distance =
        zOverlapping
        |> Array.map(fun z ->
            Array.find (fun slice -> slice.z = z ) body.slices,
            Array.find (fun slice -> slice.z = z ) couch.slices)
        |> Array.map(fun (b, c) -> abs( b.bounds.maxY - c.bounds.minY))
         

    Array.zip distance zOverlapping
    

//add an axialslice for vacfix
let vacfixSlice 
    (bodySlice : AxialSlice) 
    (couchSlice : AxialSlice)
    : AxialSlice
    =
    let gap = distanceBCSlice bodySlice couchSlice
    let extraHeigth = 80.
    
    let h = gap + extraHeigth
    let lm = 50.

    let vacfixSliceLoop = [|
        VVector(bodySlice.bounds.minX - lm, couchSlice.bounds.minY, bodySlice.z);
        VVector(bodySlice.bounds.minX - lm, couchSlice.bounds.minY - h, bodySlice.z);
        VVector(bodySlice.bounds.maxX + lm, couchSlice.bounds.minY - h, bodySlice.z);
        VVector(bodySlice.bounds.maxX + lm, couchSlice.bounds.minY, bodySlice.z)|]

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

    let VacFicSlices =
        zOverlapping
        |> Array.map(fun z -> (
            Array.find (fun b -> b.z = z) body.slices,
            Array.find (fun c -> c.z = z) couch.slices ))
        |> Array.map(fun(b, c) -> vacfixSlice b c)

    let vacfixBounds = computeBoundingBox3D VacFicSlices
    {
        slices = VacFicSlices
        sliceThickness = body.sliceThickness
        bounds = vacfixBounds
    }