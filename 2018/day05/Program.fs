open System
open VibrantCode.AdventOfCode

let run data = 
    raise (new NotImplementedException())

[<EntryPoint>]
let main argv =
    argv.[0]
    |> AdventHelpers.loadLines
    |> Seq.map int
    |> run

    0
