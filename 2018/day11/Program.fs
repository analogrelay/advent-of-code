open System.Diagnostics

// Two tricks are used here to reduce run time for part 2:
// 1. The data show a "peak" power level, so that once the power level starts decreasing, it will always decrease
//    This means once we find the first size that has a *lower* power level than it's predecessor, it's predecessor is the best result
// 2. We use a Summed-Area Table (https://en.wikipedia.org/wiki/Summed-area_table), computed once, to rapidly compute sums for particular rectangles

/// <summary>
/// Enumerates the top-left corner coordinates of all the 3x3 squares in the grid
/// </summary>
let enumerateSquares (size: int) =
    seq {
        for x in size..300 do
            for y in size..300 do
                yield (x, y)
    }

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
/// Determines the power level for a single square in the grid.
/// </summary>
/// <param name="serialNumber">The serial number for the grid.</param>
/// <param name="region">The set of squares contained in the grid.</param>
let powerLevelForSquare (sumTable: int[,]) size (x, y) =
    // Computes the sum using Summed-area table algorithm
    // See: https://en.wikipedia.org/wiki/Summed-area_table
    let sa = sumTable.[x - size, y - size]
    let sb = sumTable.[x, y - size]
    let sc = sumTable.[x - size, y]
    let sd = sumTable.[x, y]
    let res = sd + sa - sb - sc
    res

/// <summary>
/// Generates a summed-area table (https://en.wikipedia.org/wiki/Summed-area_table) for the specified serial number
/// </summary>
/// <param name="serialNumber">The serial number of the grid to generate</param>
let generateSumTable serialNumber =
    let mutable arr: int[,] = Array2D.zeroCreate 301 301
    for y in 1..300 do
        for x in 1..300 do
            let myVal = powerLevelForCell serialNumber (x, y)
            let newVal = myVal + arr.[x, y - 1] + arr.[x - 1, y] - arr.[x - 1, y - 1]
            arr.[x,y] <- newVal
    arr

let generatePowerGrid serialNumber =
    Array2D.init 300 300 (fun x y -> powerLevelForCell serialNumber (x + 1, y + 1))

let findBestSquare (sumTable: int[,]) (size: int) =
    let ((x1, y1), powerLevel) =
        enumerateSquares size
        |> Seq.map (fun coord -> (coord, powerLevelForSquare sumTable size coord))
        |> Seq.maxBy snd

    ((x1 - size + 1, y1 - size + 1), powerLevel)

    
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
    let sumTable = generateSumTable serialNumber

    let ((x1, y1), powerLevel) = findBestSquare sumTable 3
    printfn "Part 1: %d,%d (level: %d)" x1 y1 powerLevel

    let time = Stopwatch.StartNew()
    let ((x1, y1, size), powerLevel) =
        seq { 1..300 }
        |> Seq.map (fun size ->
            let ((x1, y1), powerLevel) = findBestSquare sumTable size
            ((x1, y1, size), powerLevel))
        |> firstMaximaBy snd
    let elapsed = time.ElapsedMilliseconds
    printfn "Part 2: %d,%d,%d (level: %d) (elapsed: %dms)" x1 y1 size powerLevel elapsed

[<EntryPoint>]
let main argv =
    argv.[0]
    |> int
    |> run

    0
