namespace Shared

type StructureSnapshotDto = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshDto option
    ContourSlices: ContourSliceDto list
    Bounds: Bounds3D option
}

type BodySnapshotDto = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshDto option
    ContourSlices: BodySliceDto list
    Bounds: Bounds3D option
    SliceThicknessMm: float option
}
