open System
open VibrantCode.AdventOfCode

let run data = 
    data
    |> Seq.sum
    |> printfn "Part 1: %d"

    // Cycle the provided sequence indefinitely
    let rec cycle2 s i = seq {
        printfn "Iteration: %d" i
        yield! s
        yield! cycle2 s (i + 1)
    }
    let rec cycle s = cycle2 s 0

    data
    |> cycle
    |> Seq.scan (+) 0
    |> Seq.skip 1
    |> Seq.scan (fun (last_sum, set) sum ->
        match last_sum with
        | None -> (Some(sum), Set.empty)
        | Some(l) -> (Some(sum), Set.add l set))
        (None, Set.empty)
    |> Seq.pick (fun (last_sum, set) ->
        match last_sum with
        | None -> None
        | Some(l) -> if Set.contains l set then Some(l) else None
    )
    |> printfn "Part 2: %A"

[<EntryPoint>]
let main argv =
    argv.[0]
    |> AdventHelpers.loadLines
    |> Seq.map int
    |> run

    0
