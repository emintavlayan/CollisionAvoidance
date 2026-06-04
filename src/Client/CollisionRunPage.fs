module CollisionRunPage

open Shared

type Model = {
    RunId: System.Guid option
    Summary: CollisionRunSummaryDto option
}

let init = { RunId = None; Summary = None }
