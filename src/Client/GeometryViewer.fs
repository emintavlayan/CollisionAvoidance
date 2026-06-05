module GeometryViewer

open Shared

/// Represents the detached geometry payload that a future viewer can render.
type SceneModel = {
    Body: BodySnapshotDto option
    Accessories: AccessoryModelDto list
}

let empty = { Body = None; Accessories = [] }
