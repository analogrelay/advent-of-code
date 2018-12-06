namespace VibrantCode.AdventOfCode

module AdventHelpers =
    open System.IO

    let pairwiseApply func (first, second) =
        (func first, func second)

    let split2 (chr: char) (inp: string) =
        let splat = inp.Split([| chr |], 2)
        (splat.[0].Trim(), splat.[1].Trim())

    let seqOfArray2D (arr: 'a[,]) =
        let base1 = Array2D.base1 arr
        let base2 = Array2D.base2 arr
        let end1 = (Array2D.length1 arr) - 1
        let end2 = (Array2D.length2 arr) - 1
        seq {
            for i in base1..end1 do
                for j in base2..end2 do
                    yield arr.[i, j]
        }

    let loadString (file: string) = File.ReadAllText(file)

    let loadLines (file: string) = seq {
        use stream = new StreamReader(file)
        while not stream.EndOfStream do
            yield stream.ReadLine()
    }
