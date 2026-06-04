module GeometryViewer

open Shared

type SceneModel = {
    Body: BodySnapshotDto option
    Accessories: AccessoryModelDto list
}

let empty = { Body = None; Accessories = [] }
