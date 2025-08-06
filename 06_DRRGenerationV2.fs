namespace DrrGeneration

open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types

[<AutoOpen>]
module private Internals =
    /// Window/level to 0..255 using ESAPI's display-space values.
    let inline wlToByte
        (level : float)
        (window : float)
        (displayValue : float)
        =
        let minV =
            level - window / 2.0

        let maxV =
            level + window / 2.0

        let f =
            (displayValue - minV)
            / (maxV - minV)

        let f =
            Math.Max(0.0, Math.Min(1.0, f))

        byte (f * 255.0)

    /// Save a single-plane ESAPI Image (e.g., DRR) to 8-bit grayscale PNG.
    /// Uses Image.VoxelToDisplayValue + Image.Level/Window.
    let saveSinglePlaneAsPng
        (img : Image)
        (planeIndex : int)
        (filePath : string)
        (flipVertical : bool)
        =
        let xSize, ySize =
            img.XSize, img.YSize

        if planeIndex <> 0 then
            // DRR is 2D; index is typically 0. For completeness we keep planeIndex.
            ()

        // IMPORTANT: buffer is [X, Y]
        let vox =
            Array2D.zeroCreate<int> xSize ySize

        img.GetVoxels(planeIndex, vox)

        let level =
            img.Level // in display space (same space as VoxelToDisplayValue)

        let window =
            img.Window

        use bmp =
            new Bitmap(xSize, ySize, PixelFormat.Format24bppRgb)

        let data =
            bmp.LockBits(
                Rectangle(0, 0, xSize, ySize),
                ImageLockMode.WriteOnly,
                bmp.PixelFormat
            )

        try
            let stride =
                data.Stride

            let bytes =
                stride * ySize

            let managed =
                Array.zeroCreate<byte> bytes

            for y = 0 to ySize - 1 do
                let srcY =
                    if flipVertical then (ySize - 1 - y) else y

                let row =
                    y * stride

                for x = 0 to xSize - 1 do
                    // Convert raw -> display-space via ESAPI first
                    let dv =
                        img.VoxelToDisplayValue(vox[x, srcY])

                    let g =
                        wlToByte level window dv

                    let i =
                        row + x * 3

                    managed[i] <- g // B
                    managed[i + 1] <- g // G
                    managed[i + 2] <- g // R

            Marshal.Copy(managed, 0, data.Scan0, bytes)
        finally
            bmp.UnlockBits(data)

        bmp.Save(filePath, ImageFormat.Png)

/// Options for generating/saving a DRR PNG.
type SaveOptions = {
    /// Flip vertically to match on-screen appearance if needed.
    FlipVertical : bool
    /// If true, refresh (create/replace) the DRR before reading pixels.
    RebuildDrr : bool
}

module Drr =
    /// Generate (or reuse) a DRR for the first beam in the plan and save it as PNG.
    /// - Always uses ESAPI conversions (VoxelToDisplayValue + Level/Window).
    /// - Returns Ok path or Error message.
    let SaveFirstBeamDrrPng
        (context : ScriptContext)
        (drrParams : DRRCalculationParameters)
        (filePath : string)
        (opts : SaveOptions)
        =
        try
            if isNull context.PlanSetup then
                Error "No PlanSetup in context."

            else
                let plan =
                    context.PlanSetup

                let beamOpt =
                    plan.Beams |> Seq.tryHead

                match beamOpt with
                | None -> Error "No beams in PlanSetup."
                | Some beam ->
                    // Create or refresh the DRR if requested.
                    let drrImage =
                        if opts.RebuildDrr then
                            beam.CreateOrReplaceDRR(drrParams)
                        else
                            // If DRR already exists for the beam, ESAPI returns it;
                            // if not, we create it to be safe.
                            beam.CreateOrReplaceDRR(drrParams)

                    // DRR is a 2D Image; plane index 0.
                    Internals.saveSinglePlaneAsPng
                        drrImage
                        0
                        filePath
                        opts.FlipVertical

                    Ok filePath
        with ex ->
            Error(sprintf "Failed to generate/save DRR: %s" ex.Message)

    /// Same as SaveFirstBeamDrrPng, but lets you choose a specific beam (by a simple predicate).
    let SaveDrrPngForBeam
        (context : ScriptContext)
        (chooseBeam : Beam seq -> Beam option)
        (drrParams : DRRCalculationParameters)
        (filePath : string)
        (opts : SaveOptions)
        =
        try
            if isNull context.PlanSetup then
                Error "No PlanSetup in context."

            else
                let plan =
                    context.PlanSetup

                match chooseBeam plan.Beams with
                | None -> Error "Beam selection returned no beam."
                | Some beam ->
                    let drrImage =
                        if opts.RebuildDrr then
                            beam.CreateOrReplaceDRR(drrParams)
                        else
                            beam.CreateOrReplaceDRR(drrParams)

                    Internals.saveSinglePlaneAsPng
                        drrImage
                        0
                        filePath
                        opts.FlipVertical

                    Ok filePath
        with ex ->
            Error(sprintf "Failed to generate/save DRR: %s" ex.Message)

(*
open VMS.TPS.Common.Model.API
open DrrGeneration

let drrParams = DRRCalculationParameters(
                    drrSize = 200.0,
                    weight  = 50.0,
                    ctFrom  = -500.0,
                    ctTo    = 1500.0,
                    geoFrom = -100.0,
                    geoTo   = 100.0)

let opts = { FlipVertical = false; RebuildDrr = true }

match Drr.SaveFirstBeamDrrPng context drrParams @"C:\Temp\my_drr.png" opts with
| Ok path  -> // success
| Error e  -> // handle error
*)
