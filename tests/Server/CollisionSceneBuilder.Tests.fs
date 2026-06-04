module CollisionSceneBuilder.Tests

open CollisionSceneBuilder
open Shared
open Xunit

let point3D x y z : Point3D = { X = x; Y = y; Z = z }

let point2D x y : Point2D = { X = x; Y = y }

let contour z =
    {
        Points = [ point3D 0.0 0.0 z; point3D 10.0 0.0 z; point3D 10.0 10.0 z; point3D 0.0 10.0 z ]
        Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 }
    }

let bodyStructure =
    {
        StructureId = "BODY"
        DisplayName = Some "External"
        Mesh = None
        ContourSlices = [ { Z = 0.0; Contours = [ contour 0.0 ]; Bounds = Some { Min = point2D 0.0 0.0; Max = point2D 10.0 10.0 } } ]
        Bounds = Some { Min = point3D 0.0 0.0 -1.0; Max = point3D 10.0 10.0 1.0 }
    }

let couchStructure =
    {
        StructureId = "CouchSurface"
        DisplayName = Some "Couch Surface"
        Mesh = None
        ContourSlices = [ { Z = 0.0; Contours = [ contour 0.0 ]; Bounds = Some { Min = point2D -5.0 -5.0; Max = point2D 15.0 15.0 } } ]
        Bounds = Some { Min = point3D -5.0 -5.0 -2.0; Max = point3D 15.0 15.0 2.0 }
    }

let accessoryStructure structureId =
    {
        StructureId = structureId
        DisplayName = Some structureId
        Mesh = None
        ContourSlices = []
        Bounds = None
    }

[<Fact>]
let ``BODY only volume composition succeeds`` () =
    let result = composeStructureVolumeList [ bodyStructure ]

    match result with
    | Ok composition ->
        Assert.Equal("BODY", composition.Body.StructureId)
        Assert.Empty(composition.OptionalStructures)
    | Error error ->
        failwith error

[<Fact>]
let ``BODY plus couch surface composition succeeds`` () =
    let result = composeStructureVolumeList [ bodyStructure; couchStructure ]

    match result with
    | Ok composition ->
        Assert.Single(composition.OptionalStructures) |> ignore
        Assert.Equal("CouchSurface", composition.OptionalStructures.Head.StructureId)
    | Error error ->
        failwith error

[<Fact>]
let ``Empty input fails`` () =
    let result = composeStructureVolumeList []

    match result with
    | Ok _ -> failwith "Expected empty composition input to fail."
    | Error error -> Assert.Equal("Volume composition requires BODY plus optional structures, but the input list was empty.", error)

[<Fact>]
let ``Missing BODY fails`` () =
    let result = composeStructureVolumeList [ couchStructure ]

    match result with
    | Ok _ -> failwith "Expected missing BODY composition to fail."
    | Error error -> Assert.Equal("Volume composition requires a BODY structure.", error)

[<Fact>]
let ``Multiple optional structures can be passed as a list even if not clinically used yet`` () =
    let result = composeStructureVolumeList [ bodyStructure; couchStructure; accessoryStructure "VacFixPlaceholder"; accessoryStructure "BreastBoardPlaceholder" ]

    match result with
    | Ok composition ->
        Assert.Equal(3, composition.OptionalStructures.Length)
        Assert.Equal(4, composition.Structures.Length)
    | Error error ->
        failwith error
