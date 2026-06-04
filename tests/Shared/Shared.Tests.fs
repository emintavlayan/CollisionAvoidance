module Shared.Tests

open Shared
open Xunit

[<Fact>]
let ``Todo validation rejects an empty description`` () =
    Assert.False(Todo.isValid "")
