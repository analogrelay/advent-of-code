namespace VibrantCode.AdventOfCode

module AdventHelpers =
    open System.IO

    let loadString (file: string) = File.ReadAllText(file)

    let loadLines (file: string) = seq {
        use stream = new StreamReader(file)
        while not stream.EndOfStream do
            yield stream.ReadLine()
    }
