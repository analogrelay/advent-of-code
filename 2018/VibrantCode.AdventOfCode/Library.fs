namespace VibrantCode.AdventOfCode

open System.Text.RegularExpressions

module AdventHelpers =
    open System.IO

    // Active pattern for matching using a regular expression.
    // Produces a list of groups yielded by the regex, skipping the first one
    let (|Regex|_|) (pattern: string) (input: string) =
        let matches = Regex.Match(input, pattern)
        if matches.Success then
            Some([ for g in matches.Groups do yield g.Value ] |> List.skip 1)
        else
            None

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
                    yield ((i, j), arr.[i, j])
        }

    let loadString (file: string) = File.ReadAllText(file)

    let loadSpaceDelimited (file: string) =
        (loadString file).Split(' ') |> Seq.ofArray

    let loadLines (file: string) = seq {
        use stream = new StreamReader(file)
        while not stream.EndOfStream do
            yield stream.ReadLine()
    }
