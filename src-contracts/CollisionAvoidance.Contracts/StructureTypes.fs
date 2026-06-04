namespace Shared

type BodySnapshotDto = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshDto option
    ContourSlices: ContourSliceDto list
    Bounds: Bounds3D option
}
