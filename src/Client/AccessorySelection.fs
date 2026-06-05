module AccessorySelection

open Shared

/// Represents the client-side accessory-selection state for future scene composition controls.
type Model = { SelectedAccessoryIds: Set<string> }

let init (accessories: AccessoryModelDto list) = {
    SelectedAccessoryIds =
        accessories
        |> List.filter (fun accessory -> accessory.IsEnabled)
        |> List.map (fun accessory -> accessory.AccessoryId)
        |> Set.ofList
}
