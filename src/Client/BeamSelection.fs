module BeamSelection

open Shared

type Model = { SelectedBeamIds: Set<string> }

let init (plan: PlanSnapshotDto) = {
    SelectedBeamIds = plan.Beams |> List.map (fun beam -> beam.BeamId) |> Set.ofList
}
