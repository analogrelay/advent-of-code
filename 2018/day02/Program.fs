open System
open VibrantCode.AdventOfCode

let hasRepeat (target: int) (str: string) =
    str
    |> Seq.countBy id
    |> Seq.exists (fun (chr, count) -> count = target)

let diffStrings (left: string) (right: string) =
    Seq.zip left right
    |> Seq.map (fun (x, y) -> x <> y)

let part1 data =
    let getCounts (twos, threes) boxId =
        let newTwos = if hasRepeat 2 boxId then twos + 1 else twos
        let newThrees = if hasRepeat 3 boxId then threes + 1 else threes
        (newTwos, newThrees)

    let (twos, threes) = data |> Seq.fold getCounts (0, 0)
    twos * threes |> printfn "Part 1: %d"

let buildString diff (boxId: string) =
    Seq.zip diff boxId
    |> Seq.filter (fun (x, _) -> not x)
    |> Seq.map (fun (_, y) -> y.ToString())
    |> String.concat ""

let part2 data =
    let data = List.ofSeq data
    data
    |> Seq.pick (fun boxId ->
        // Diff with everything else
        data
        |> Seq.tryPick (fun otherId ->
            let diff = diffStrings boxId otherId
            let count = diff |> Seq.filter id |> Seq.length
            if count = 1 then
                Some(buildString diff boxId)
            else
                None
        )
    )
    |> printfn "Part 2: %s"

let run data =
    data |> part1
    data |> part2

[<EntryPoint>]
let main argv =
    argv.[0]
    |> AdventHelpers.loadLines
    |> run
    0
