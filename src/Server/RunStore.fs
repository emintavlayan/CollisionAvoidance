module RunStore

open Shared

type CollisionRunRecord = {
    Request: CollisionRunRequestDto
    Summary: CollisionRunSummaryDto option
}

let create request = {
    Request = request
    Summary = None
}
