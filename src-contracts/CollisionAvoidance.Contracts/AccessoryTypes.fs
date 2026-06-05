namespace Shared

/// Represents the detached accessory category used when composing a collision scene.
type AccessoryKindDto =
    | CouchBase
    | CouchSurface
    | VacFix
    | BreastBoard
    | Other of string

/// Represents one detached accessory model with optional mesh, optional structure, and optional bounds.
type AccessoryModelDto = {
    AccessoryId: string
    Kind: AccessoryKindDto
    DisplayName: string
    Mesh: MeshDto option
    Structure: StructureSnapshotDto option
    Bounds: Bounds3D option
    Offset: Vector3D option
    IsEnabled: bool
}

/// Represents one detached couch-base wrapper.
type CouchBaseDto = { Model: AccessoryModelDto }

/// Represents one detached VacFix wrapper.
type VacFixDto = { Model: AccessoryModelDto }

/// Represents one detached breast-board wrapper.
type BreastBoardDto = { Model: AccessoryModelDto }
