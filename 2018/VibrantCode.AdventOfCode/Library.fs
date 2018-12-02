namespace VibrantCode.AdventOfCode

module AdventHelpers =
    open System.IO

    let loadLines (file: string) = seq {
        use stream = new StreamReader(file)
        while not stream.EndOfStream do
            yield stream.ReadLine()
    }
