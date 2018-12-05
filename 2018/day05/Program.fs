open System
open VibrantCode.AdventOfCode.AdventHelpers

let charMatch (l: char) (r: char) = ((Char.ToLower l) = (Char.ToLower r))
let areReactive (l: char) (r: char) = l <> r && (charMatch l r)

let simplify polymer =
    let folder (chr: char) (polymer: char list) = 
        match (polymer, chr) with
        // Check if the front of the polymer matches the current char
        | (x :: xs, y) when areReactive x y -> xs
        | (xs, y) -> y :: xs
    Seq.foldBack folder polymer [] |> Seq.ofList

let removeAllInstances (chr: char) (data: char seq) =
    // f >> g ==> g(f(x))
    // so this wraps the charEqualIgnoreCase result in a call to "not", to negate it.
    data |> Seq.filter ((charMatch chr) >> not)

let run (data: char list) =
    let simplified = simplify data

    simplified
    |> Seq.length
    |> printfn "Part 1: %d"

    seq { 'a'..'z' }
    |> Seq.map (fun x -> (x, simplified))
    |> Seq.map (fun (chr, data) -> (chr, removeAllInstances chr data))
    |> Seq.map (fun (chr, data) -> (chr, simplify data |> Seq.length))
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
