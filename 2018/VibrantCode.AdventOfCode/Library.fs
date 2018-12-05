namespace VibrantCode.AdventOfCode

module AdventHelpers =
    open System.IO

    let apply2 op (x, y) = op x y
    let applyToFst func (first, second) = (func first, second)
    let applyToSnd func (first, second) = (first, func second)
    let loadString (file: string) = File.ReadAllText(file)

    let loadLines (file: string) = seq {
        use stream = new StreamReader(file)
        while not stream.EndOfStream do
            yield stream.ReadLine()
    }
