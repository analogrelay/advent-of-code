open System
open VibrantCode.AdventOfCode.AdventHelpers

// With the test data, I observed that there's a "peak" to the powerlevels by size. So,
// the best powerlevel is the first "maxima" that is found in order. So as the size increases,
// so does the powerlevel, but as soon as it drops, we have the final answer (the value immediately
// before the drop).

/// <summary>
/// Enumerates the top-left corner coordinates of all the 3x3 squares in the grid
/// </summary>
let enumerateSquares (size: int) =
    let edge = 300 - (size - 1);
    seq {
        for x in 1..edge do
            for y in 1..edge do
                yield (x, y)
    }

/// <summary>
/// Generates all the cells contained within the square identified by the cell at the specified coordinate
/// </summary>
/// <param name="x">The x coordinate of the top-level cell in the square.</param>
/// <param name="y">The y coordinate of the top-level cell in the square.</param>
let regionForCell size (x0, y0) =
    let xend = (x0 + size) - 1
    let yend = (y0 + size) - 1
    seq {
        for x in x0..xend do
            for y in y0..yend do
                yield (x, y)
    } |> List.ofSeq

/// <summary>
/// Extract the hundreds digit for a number.
/// </summary>
/// <param name="x">The number to extract the hundreds digit for.</param>
let hundredsDigit x = ((x / 100) % 10)

/// <summary>
/// Determines the power level for a single cell.
/// </summary>
/// <param name="serialNumber">The serial number for the grid.</param>
/// <param name="x">The x coordinate of the cell.</param>
/// <param name="y">The y coordinate of the cell.</param>
let powerLevelForCell serialNumber (x, y) =
    let rackId = x + 10
    let step2 = rackId * y
    let step3 = step2 + serialNumber
    let step4 = step3 * rackId
    let step5 = hundredsDigit step4
    step5 - 5

/// <summary>
/// Determines the power level for a single 3x3 square in the grid.
/// </summary>
/// <param name="serialNumber">The serial number for the grid.</param>
/// <param name="region">The set of squares contained in the grid.</param>
let powerLevelForSquare serialNumber size (x, y) =
    regionForCell size (x, y) |> Seq.sumBy (powerLevelForCell serialNumber)

let firstMaximaBy func seq =
    let mutable current = None
    let mutable currentVal = None
    let picker item =
        let candidate = func item
        match currentVal with
        | Some(c) when candidate < c ->
            // We're done
            current
        | _ ->
            current <- Some(item)
            currentVal <- Some(candidate)
            None

    seq |> Seq.pick picker

let run (serialNumber: int) = 
    printfn "Serial Number: %d" serialNumber
    let ((x, y), powerLevel) =
        enumerateSquares 3
        |> Seq.map (fun regionId -> (regionId, powerLevelForSquare serialNumber 3 regionId))
        |> Seq.maxBy snd
    printfn "Part 1: %d (at %d, %d)" powerLevel x y

    let ((x, y, size), powerLevel) =
        seq { 1..300 }
        |> Seq.map (fun size ->
            let ((x, y), value) =
                enumerateSquares size
                |> Seq.map (fun regionId -> (regionId, powerLevelForSquare serialNumber size regionId))
                |> Seq.maxBy snd
            printfn "Max for size %2d: %d (at %d, %d)" size value x y
            ((x, y, size), value))
        |> firstMaximaBy snd
    printfn "Part 2: %d (at %d, %d, %d)" powerLevel x y size

[<EntryPoint>]
let main argv =
    argv.[0]
    |> int
    |> run

    0
