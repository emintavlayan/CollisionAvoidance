module AccessoryModels

open Shared

let enabledAccessories (accessories: AccessoryModelDto list) =
    accessories |> List.filter (fun accessory -> accessory.IsEnabled)
