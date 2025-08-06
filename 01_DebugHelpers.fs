(**
    Module: DebugHelpers
    Purpose: 
        - Provides helper functions for debugging and logging within the VMS TPS scripting environment,
        - including message box display and log file writing utilities.
    Why:
        - Centralizes debug-related utilities to avoid duplication,
        - makes scripts easier to troubleshoot and maintain,
        - ensures consistent debug output and logging practices.

    Notes:
        - All functions here are designed to be side-effect-safe where possible.
        - Depends on VMS.TPS.Common.Model.API for core types.
*)

module VMS.TPS.DebugHelpers

open System.IO

/// Shows the given message for debugging purposes
let showMessageBox (message : string) =
    System.Windows.Forms.MessageBox.Show(message)
    |> ignore

/// Logs a message to a specified log file
let logMessage (logFilePath : string) (message : string) =
    use writer =
        File.AppendText(logFilePath)

    writer.WriteLine($"{System.DateTime.Now:u} - {message}")
