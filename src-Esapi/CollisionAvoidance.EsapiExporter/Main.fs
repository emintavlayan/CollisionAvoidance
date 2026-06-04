module CollisionAvoidance.EsapiExporter.Main

open CollisionAvoidance.EsapiExporter.SafeServerClient

/// Starts the ESAPI exporter entry point placeholder without invoking real ESAPI behavior yet.
[<EntryPoint>]
let main _argv =
    let _ = defaultServerBaseUrl
    0
