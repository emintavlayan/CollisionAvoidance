module CollisionAvoidance.EsapiExporter.PatientIdObfuscation

open System.Security.Cryptography
open System.Text

/// Obfuscates a patient id deterministically so raw identifiers are never sent in request DTOs.
let obfuscatePatientId (patientId: string) =
    use sha256 = SHA256.Create()
    patientId
    |> Encoding.UTF8.GetBytes
    |> sha256.ComputeHash
    |> Array.map (fun value -> value.ToString("x2"))
    |> String.concat ""
