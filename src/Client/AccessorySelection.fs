module AccessorySelection

open Shared

type Model = { SelectedAccessoryIds: Set<string> }

let init (accessories: AccessoryModelDto list) = {
    SelectedAccessoryIds =
        accessories
        |> List.filter (fun accessory -> accessory.IsEnabled)
        |> List.map (fun accessory -> accessory.AccessoryId)
        |> Set.ofList
}
