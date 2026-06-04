module CollisionAvoidance.EsapiExporter.SafeServerClient

open System
open System.Diagnostics
open Shared

let defaultServerBaseUrl = Uri("http://172.31.245.206:8080")

/// Posts a detached collision run request to the SAFE server and returns the created run page URL.
let postCollisionRunRequest (_serverBaseUrl: Uri) (_request: CollisionRunRequestDto) : Async<Uri> =
    async { return failwith "TODO: Serialize and POST CollisionRunRequestDto to the SAFE server." }

/// Opens the collision run page in the user's default browser.
let openCollisionRunPage (runPageUrl: Uri) =
    let startInfo = ProcessStartInfo(runPageUrl.AbsoluteUri)
    startInfo.UseShellExecute <- true
    Process.Start(startInfo) |> ignore
