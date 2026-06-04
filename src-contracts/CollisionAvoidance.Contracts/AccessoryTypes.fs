namespace Shared

type AccessoryKindDto =
    | CouchBase
    | CouchSurface
    | VacFix
    | BreastBoard
    | Other of string

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

type CouchBaseDto = { Model: AccessoryModelDto }

type VacFixDto = { Model: AccessoryModelDto }

type BreastBoardDto = { Model: AccessoryModelDto }
