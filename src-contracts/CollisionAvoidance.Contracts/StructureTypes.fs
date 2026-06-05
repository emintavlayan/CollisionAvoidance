namespace Shared

/// Represents one detached non-BODY structure snapshot with contours, optional mesh, and optional bounds.
type StructureSnapshotDto = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshDto option
    ContourSlices: ContourSliceDto list
    Bounds: Bounds3D option
}

/// Represents one detached BODY snapshot with contours, optional mesh, optional bounds, and optional slice thickness.
type BodySnapshotDto = {
    StructureId: string
    DisplayName: string option
    Mesh: MeshDto option
    ContourSlices: BodySliceDto list
    Bounds: Bounds3D option
    SliceThicknessMm: float<mm> option
}
