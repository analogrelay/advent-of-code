open System
open VibrantCode.AdventOfCode.AdventHelpers

let hasRepeat (target: int) (str: string) =
    str
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.exists ((=) target)
    
let diffStrings (left: string) (right: string) =
    Seq.zip left right
    |> Seq.map (apply2 (<>))

let part1 data =
    let getCounts (twos, threes) boxId =
        let newTwos = if hasRepeat 2 boxId then twos + 1 else twos
        let newThrees = if hasRepeat 3 boxId then threes + 1 else threes
        (newTwos, newThrees)

    data
    |> Seq.fold getCounts (0, 0)
    |> (apply2 (*))
    |> printfn "Part 1: %d"

let buildString diff (boxId: string) =
    Seq.zip diff boxId
    |> Seq.filter (fst >> not)
    |> Seq.map (snd >> string)
    |> String.concat ""

let part2 data =
    let data = List.ofSeq data
    data
    |> Seq.pick (fun boxId ->
        // Diff with everything else
        data
        |> Seq.tryPick (fun otherId ->
            let diff = diffStrings boxId otherId
            if diff |> Seq.filter id |> Seq.length = 1 then
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
    |> loadLines
    |> run
    0
