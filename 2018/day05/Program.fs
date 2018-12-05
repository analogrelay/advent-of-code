open System
open VibrantCode.AdventOfCode.AdventHelpers
open System
open System

let charEqualIgnoreCase (l: char) (r: char) = ((Char.ToLower l) = (Char.ToLower r))
let areReactive (l: char) (r: char) = l <> r && (charEqualIgnoreCase l r)

let rec countNonReactiveWorker (count: int) (lookbehind: char list) (list: char list) = 
    // We maintain two lists, the main list and the lookbehind list
    // The lookbehind list is in _REVERSE_ order (so the head of the list is the
    // immediate previous item)

    match (lookbehind, list) with
    // There's a match!
    // Deduct one from the count and continue
    | (l :: lbtail, r :: tail) when areReactive l r -> countNonReactiveWorker (count - 1) lbtail tail

    // No matches, move forward
    | (lb, head :: tail) -> countNonReactiveWorker (count + 1) (head :: lb) tail

    // Empty list, end case.
    | (_, []) -> count 

let countNonReactive (data: char list) =
    countNonReactiveWorker 0 [] data

let removeAllInstances (chr: char) (data: char list) =
    // f >> g ==> g(f(x))
    // so this wraps the charEqualIgnoreCase result in a call to "not", to negate it.
    data |> List.filter ((charEqualIgnoreCase chr) >> not)

let run (data: char list) =
    data
    |> countNonReactive
    |> printfn "Part 1: %d"

    seq { 'a'..'z' }
    |> Seq.map (fun x -> (x, data))
    |> Seq.map (fun (chr, data) -> (chr, removeAllInstances chr data))
    |> Seq.map (applyToSnd countNonReactive)
    |> Seq.minBy snd
    |> snd
    |> printfn "Part 2: %d"

[<EntryPoint>]
let main argv =
    argv.[0]
    |> loadString
    |> List.ofSeq
    |> run

    0
