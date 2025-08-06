module VMS.TPS.DrrGeneration

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types
open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices

let drrSize =
    200.0

let weight =
    50.0

let ctFrom =
    -500.0

let ctTo =
    1500.0

let geoFrom =
    -100.0

let geoTo =
    100.0

let drrParams =
    DRRCalculationParameters(drrSize, weight, ctFrom, ctTo, geoFrom, geoTo)

/// Scale raw voxel to 0..255 using window/level from the ESAPI Image (internal scale).
let private wlToByte (level : float) (window : float) (v : int) =
    let minV =
        level - window / 2.0

    let maxV =
        level + window / 2.0

    let f =
        (float v - minV)
        / (maxV - minV)

    let f =
        Math.Max(0.0, Math.Min(1.0, f))

    byte (f * 255.0)

/// Save a 2D ESAPI Image (e.g., DRR) plane 0 as PNG (grayscale in 24bpp).
let private saveImagePlane0AsPng
    (img : VMS.TPS.Common.Model.API.Image)
    (filePath : string)
    =
    let xSize, ySize =
        img.XSize, img.YSize

    // IMPORTANT: buffer is [X, Y]
    let vox =
        Array2D.zeroCreate<int> xSize ySize

    img.GetVoxels(0, vox)

    let level =
        img.Level

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

        // If your saved image appears vertically flipped compared to Eclipse,
        // switch 'y' to (ySize-1 - y).
        for y = 0 to ySize - 1 do
            let row =
                y * stride

            for x = 0 to xSize - 1 do
                let g =
                    wlToByte level window vox[x, y]

                let i =
                    row + x * 3

                managed[i] <- g // B
                managed[i + 1] <- g // G
                managed[i + 2] <- g // R

        Marshal.Copy(managed, 0, data.Scan0, bytes)
    finally
        bmp.UnlockBits(data)

    bmp.Save(filePath, ImageFormat.Png)

/// Create (or replace) the DRR for the first beam and save to PNG.
/// Returns Ok path or Error message.
let trySaveDrrPng (context : ScriptContext) (filePath : string) =
    try
        match context.PlanSetup with
        | null -> Error "No PlanSetup in context."
        | plan ->
            match plan.Beams |> Seq.tryHead with
            | None -> Error "No beams found in the plan setup."
            | Some beam ->
                // Build/refresh DRR for this beam
                let drrImage =
                    beam.CreateOrReplaceDRR(drrParams)

                // Write plane 0 to file
                saveImagePlane0AsPng drrImage filePath
                Ok filePath
    with ex ->
        Error(sprintf "Failed to generate/save DRR: %s" ex.Message)
