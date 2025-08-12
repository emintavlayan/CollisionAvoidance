(**
    Module: DrrGeneration
    Purpose:
        - Provides a function to create Digitally Reconstructed Radiographs (DRRs) 
          from a given treatment beam using ESAPI's DRRCalculationParameters.
    Why:
        - Encapsulates DRR creation logic in one place for clarity and maintainability.
        - Ensures all required DRR parameters are set explicitly.
    Notes:
        - Produces a single 2D grayscale image based on beam geometry and HU clipping.
        - Uses ESAPI's CreateOrReplaceDRR API method.
        - No post-processing or export is handled here; see DrrExporting module for that.
*)

module VMS.TPS.DrrGeneration

open VMS.TPS.Common.Model.API
open VMS.TPS.Common.Model.Types

/// Parameters required for DRR creation.
type DRROptions = {
    DrrSizeMM : float
    Weight : float
    CTFrom : float
    CTTo : float
    GeoFrom : float
    GeoTo : float
}

/// Creates a DRR for the given beam using the provided parameters.
let createDRR (beam : Beam) (opts : DRROptions) : Result<Image, string> =
    try
        // Build DRR calculation parameters in one go
        let p =
            DRRCalculationParameters(
                opts.DrrSizeMM,
                opts.Weight,
                opts.CTFrom,
                opts.CTTo,
                opts.GeoFrom,
                opts.GeoTo
            )

        let drrImage =
            beam.CreateOrReplaceDRR(p)

        if isNull (box drrImage) then
            Error "CreateOrReplaceDRR returned null image."
        else
            Ok drrImage
    with ex ->
        Error(sprintf "DRR creation failed: %s" ex.Message)
