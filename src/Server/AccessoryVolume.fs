module AccessoryVolume

open Shared

/// Returns the enabled accessory structures that can later contribute to collision volume composition.
let getEnabledAccessoryStructures (accessories: AccessoryModelDto list) =
    accessories
    |> List.filter (fun accessory -> accessory.IsEnabled)
    |> List.choose (fun accessory -> accessory.Structure)
