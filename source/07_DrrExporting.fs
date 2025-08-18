(**
    Module: DrrExporting
    Purpose:
        - Provides multiple ways to export a DRR image produced by ESAPI:
            1) Save to the filesystem as an 8-bit grayscale PNG.
            2) Convert to a 2D byte array for direct in-memory visualization (e.g., Plotly.NET).
            3) Convert to a JPEG byte array for sending to external services (e.g., YOLO detection server).
    Why:
        - Keeps DRR export logic separate from DRR generation for modularity.
        - Enables both file-based and in-memory workflows without code duplication.
    Notes:
        - Assumes the DRR is already pre-rendered and scaled by ESAPI.
        - No additional window/level adjustments are applied.
        - PNG saving automatically creates directories if needed.
        - JPEG encoding is optimized for network transfer.
*)

module VMS.TPS.DrrExporting

open System
open System.IO
open System.Drawing
open System.Drawing.Imaging
open VMS.TPS.Common.Model.API

/// Saves a DRR image exactly as rendered by ESAPI to a PNG file.
let saveAsPng (image : Image) (outputPath : string) : Result<string, string> =
    try
        let width =
            image.XSize

        let height =
            image.YSize

        // DRR pixels are already pre-windowed & rendered, so just copy them to an 8-bit bitmap.
        let plane =
            Array2D.zeroCreate<int> height width

        image.GetVoxels(0, plane)

        use bmp =
            new Bitmap(width, height, PixelFormat.Format8bppIndexed)

        // Set grayscale palette
        let pal =
            bmp.Palette

        for i = 0 to 255 do
            pal.Entries.[i] <- Color.FromArgb(i, i, i)

        bmp.Palette <- pal

        let rect =
            Rectangle(0, 0, width, height)

        let data =
            bmp.LockBits(rect, ImageLockMode.WriteOnly, bmp.PixelFormat)

        try
            let stride =
                data.Stride

            let mutable ptr =
                data.Scan0

            let rowBytes : byte[] =
                Array.zeroCreate width

            for y = 0 to height - 1 do
                for x = 0 to width - 1 do
                    let v =
                        plane.[y, x]

                    rowBytes.[x] <-
                        if v < 0 then 0uy
                        elif v > 255 then 255uy
                        else byte v

                System.Runtime.InteropServices.Marshal.Copy(
                    rowBytes,
                    0,
                    ptr,
                    width
                )

                ptr <- IntPtr(ptr.ToInt64() + int64 stride)
        finally
            bmp.UnlockBits(data)

        Directory.CreateDirectory(Path.GetDirectoryName outputPath)
        |> ignore

        bmp.Save(outputPath, ImageFormat.Png)
        Ok outputPath
    with ex ->
        Error(sprintf "Failed to save DRR as PNG: %s" ex.Message)

/// Converts a DRR to a 2D byte array for visualization (e.g., Plotly.NET)
let toByteArray2D (image : Image) : byte[,] =
    let width =
        image.XSize

    let height =
        image.YSize

    let plane =
        Array2D.zeroCreate<int> height width

    image.GetVoxels(0, plane)

    let bytes =
        Array2D.zeroCreate<byte> height width

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            let v =
                plane.[y, x]

            bytes.[y, x] <-
                if v < 0 then 0uy
                elif v > 255 then 255uy
                else byte v

    bytes

/// Converts a DRR to a JPEG byte array for sending to a YOLO detection server
let toJpegBytes (image : Image) : byte[] =
    let width =
        image.XSize

    let height =
        image.YSize

    let plane =
        Array2D.zeroCreate<int> height width

    image.GetVoxels(0, plane)

    use bmp =
        new Bitmap(width, height, PixelFormat.Format8bppIndexed)

    let pal =
        bmp.Palette

    for i = 0 to 255 do
        pal.Entries.[i] <- Color.FromArgb(i, i, i)

    bmp.Palette <- pal

    let rect =
        Rectangle(0, 0, width, height)

    let data =
        bmp.LockBits(rect, ImageLockMode.WriteOnly, bmp.PixelFormat)

    try
        let stride =
            data.Stride

        let mutable ptr =
            data.Scan0

        let rowBytes : byte[] =
            Array.zeroCreate width

        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                let v =
                    plane.[y, x]

                rowBytes.[x] <-
                    if v < 0 then 0uy
                    elif v > 255 then 255uy
                    else byte v

            System.Runtime.InteropServices.Marshal.Copy(rowBytes, 0, ptr, width)
            ptr <- IntPtr(ptr.ToInt64() + int64 stride)
    finally
        bmp.UnlockBits(data)

    use ms =
        new MemoryStream()

    bmp.Save(ms, ImageFormat.Jpeg)
    ms.ToArray()
