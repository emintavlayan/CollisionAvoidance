module testStructureSnapshot

open FsUnit.Xunit

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open VMS.TPS.StructureSnapshot

// Initial try of an Xunit test for StrucureSnapshot functions



let tester (x : float) : float = x*2.


let ``tester should return the float 2*x`` () = 
    tester(10.) |> should equal 20.

// Test axialslice
let testSliceZ = 2.1
let testSliceBounds = {
        minX = 0.
        maxX = 1.
        minY = 0.
        maxY = 1.
    }
let testsliceLoop = 
    [|VVector(testSliceBounds.minX,testSliceBounds.minY,testSliceZ); 
    VVector(testSliceBounds.minX,testSliceBounds.maxY,testSliceZ); 
    VVector(testSliceBounds.maxX,testSliceBounds.maxY,testSliceZ); 
    VVector(testSliceBounds.maxX,testSliceBounds.minY,testSliceZ)|]

let testSlice : AxialSlice =
    {
        z = testSliceZ
        loop = testsliceLoop
        bounds = testSliceBounds
    }

let ``AxialSlice.z should return its z value`` () =
    testSlice.z |> should equal testSliceZ

let ``AxialSlice.loop should return an array with vector points`` () =
    testSlice.loop |> should equal testsliceLoop 

let ``AxialSloce.bounds should retun the bounds of the loop`` () =
    testSlice.bounds |> should equal testSliceBounds

// Test volume


// Find how to test with beam 


// Test hull

//add additional points to the loop and randomizes their order
let testHullLoop =
    [|VVector(0.2, 0.4,testSliceZ); VVector(0.5,0.2,testSliceZ); VVector(0.7,0.1,testSliceZ); VVector(0.1,0.3,testSliceZ)|]
    |> Array.append testsliceLoop
    |> Array.randomShuffle

let testHullSlice : AxialSlice = 
    {
        z = testSliceZ 
        loop = testHullLoop
        bounds = testSliceBounds
    }
// Test findHullOfSlice
let testHull = findHullOfSlice testHullSlice

let testHullLoop2 = 
    testHull.loop
    |> Array.sortBy(fun v -> v.x, v.y)


let ``The convex hull should should only contain the outer parts`` () =
    testHullLoop2 |> should equal (Array.sortBy(fun v -> v.x, v.y) testHullLoop : VVector array)



// Test findHullOfTwoSlices


// Test findHullOfVolume


// Test findHullOfTwoVolumes


