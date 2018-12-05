open System
open VibrantCode.AdventOfCode
open System

let areReactive (l: char) (r: char) = l <> r && ((Char.ToLower l) = (Char.ToLower r))

let rec processList (count: int) (lookbehind: char list) (data: char list) = 
    // We maintain two lists, the main list and the lookbehind list
    // The lookbehind list is in _REVERSE_ order (so the head of the list is the
    // immediate previous item)

    match (lookbehind, data) with
    // There's a match!
    // Deduct one from the count and continue
    | (l :: lbtail, r :: tail) when areReactive l r -> processList (count - 1) lbtail tail

    // No matches, move forward
    | (lb, head :: tail) -> processList (count + 1) (head :: lb) tail

    // Empty list, end case.
    | (_, []) -> count 

let run (data: char list) =
    data
    |> processList 0 []
    |> printfn "Part 1: %d"

[<EntryPoint>]
let main argv =
    argv.[0]
    |> AdventHelpers.loadString
    |> List.ofSeq
    |> run

    0
