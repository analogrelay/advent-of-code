func day01(_ args: ArraySlice<String>) throws {
    guard let input = args.first else {
        throw SimpleError.error("Usage: adventofcode 1 <input>")
    }
    let data = try parseIntLines(path: input)

    var previous: Int? = nil
    var previousSum: Int? = nil

    var measurementsThatIncrease = 0
    var windowsThatIncrease = 0

    for (idx, val) in data.enumerated() {
        if let x = previous, val > x {
            measurementsThatIncrease += 1
        }

        if idx > 1 {
            let sum = data[idx - 2] + data[idx - 1] + val
            if let x = previousSum, sum > x {
                windowsThatIncrease += 1
            }
            previousSum = sum
        }

        previous = val
    }
    print("There were \(measurementsThatIncrease) occasions where the depth increased.")
    print("There were \(windowsThatIncrease) occasions where the three-measurement sum increased.")
}