module CollisionApi

open Shared

type CreateCollisionRun = CollisionRunRequestDto -> Async<CollisionRunSummaryDto>
