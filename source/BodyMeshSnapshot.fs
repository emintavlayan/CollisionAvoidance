
module VMS.TPS.BodyMeshSnapshot

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open System.Windows.Media.Media3D
open FsToolkit.ErrorHandling



/// Represents a frozen detached mesh copied from an ESAPI body structure.
type BodyMeshSnapshot =
    private
        {
            Mesh : MeshGeometry3D
        }

/// Creates a frozen mesh snapshot that can be read outside the ESAPI STA context.
let create (mesh: MeshGeometry3D) =
    result {
        let! sourceMesh =
            match isNull mesh with
            | true -> Error "Body mesh was null"
            | false -> Ok mesh

        let clonedMesh = sourceMesh.Clone()

        match clonedMesh.CanFreeze with
            | false -> return! Error "Body mesh clone could not be frozen"
            | true -> 
                clonedMesh.Freeze()
                return { Mesh = clonedMesh }
    }


/// Gets the frozen mesh from the body mesh snapshot.
let value snapshot = 
    snapshot.Mesh