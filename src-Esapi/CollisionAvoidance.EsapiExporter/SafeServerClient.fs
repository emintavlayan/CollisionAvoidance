module CollisionAvoidance.EsapiExporter.SafeServerClient

open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Shared

/// Represents the SAFE submission result returned to the ESAPI exporter workflow.
type CollisionRunSubmissionResult = {
    RunId: Guid option
    RunPageUrl: Uri option
    RawResponse: string option
}

let defaultServerBaseUrl = Uri("http://172.31.245.206:8080")

/// Creates JSON serializer options that can handle F# records, lists, options, and unions.
let createJsonSerializerOptions () =
    let options = JsonSerializerOptions(WriteIndented = true)
    options.Converters.Add(JsonFSharpConverter())
    options

/// Serializes a detached collision run request into JSON text.
let serializeCollisionRunRequest (request: CollisionRunRequestDto) =
    JsonSerializer.Serialize(request, createJsonSerializerOptions ())

/// Deserializes a SAFE create-run response payload from JSON text.
let deserializeCreateCollisionRunResponse (responseText: string) : Result<CreateCollisionRunResponseDto, string> =
    try
        let response = JsonSerializer.Deserialize<CreateCollisionRunResponseDto>(responseText, createJsonSerializerOptions ())
        Ok response
    with ex ->
        Error $"Failed to parse SAFE server response JSON: {ex.Message}"

/// Resolves the SAFE run page URL from a create-run response and server base URL.
let resolveRunPageUrl (serverBaseUrl: Uri) (response: CreateCollisionRunResponseDto) =
    match response.RunUrl with
    | Some runUrl ->
        match Uri.TryCreate(serverBaseUrl, runUrl) with
        | true, uri -> Some uri
        | _ -> None
    | None -> Some(Uri(serverBaseUrl, $"/collision/{response.RunId}"))

/// Writes a detached collision run request to a local JSON file and returns the path.
let writeCollisionRunRequestToJsonFile (outputDirectory: string) (request: CollisionRunRequestDto) : Result<string, string> =
    try
        Directory.CreateDirectory(outputDirectory) |> ignore

        let filePath =
            Path.Combine(
                outputDirectory,
                sprintf "collision-run-request-%s.json" (DateTime.UtcNow.ToString("yyyyMMdd-HHmmss"))
            )

        File.WriteAllText(filePath, serializeCollisionRunRequest request, Encoding.UTF8)
        Ok filePath
    with ex ->
        Error $"Failed to write collision request JSON: {ex.Message}"

/// Posts a detached collision run request to the SAFE server and returns any submitted run metadata.
let postCollisionRunRequest (serverBaseUrl: Uri) (request: CollisionRunRequestDto) : Async<Result<CollisionRunSubmissionResult, string>> =
    async {
        try
            use httpClient = new HttpClient(BaseAddress = serverBaseUrl)
            let json = serializeCollisionRunRequest request
            use content = new StringContent(json, Encoding.UTF8, "application/json")
            let! response = httpClient.PostAsync("/api/collision-runs", content) |> Async.AwaitTask
            let! responseText = response.Content.ReadAsStringAsync() |> Async.AwaitTask

            if response.IsSuccessStatusCode then
                match deserializeCreateCollisionRunResponse responseText with
                | Ok createResponse ->
                    return
                        Ok {
                            RunId = Some createResponse.RunId
                            RunPageUrl = resolveRunPageUrl serverBaseUrl createResponse
                            RawResponse = Some responseText
                        }
                | Error error ->
                    return Error error
            else
                return
                    Error $"SAFE server returned {(int response.StatusCode)}: {responseText}"
        with ex ->
            return Error $"Failed to post collision run request: {ex.Message}"
    }

/// Opens the collision run page in the user's default browser.
let openCollisionRunPage (runPageUrl: Uri) : Result<unit, string> =
    try
        let startInfo = ProcessStartInfo(runPageUrl.AbsoluteUri)
        startInfo.UseShellExecute <- true
        Process.Start(startInfo) |> ignore
        Ok ()
    with ex ->
        Error $"Failed to open the SAFE run page: {ex.Message}"
