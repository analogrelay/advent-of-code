//
// Created by Andrew Stanton-Nurse on 12/3/21.
//

import Foundation

struct Coordinate {
    var depth = 0
    var position = 0
}

extension Coordinate {
    static func +(left: Coordinate, right: Coordinate) -> Coordinate {
        Coordinate(depth: left.depth + right.depth, position: left.position + right.position)
    }
}

class Submarine {
    var location = Coordinate()
    var aim = 0

    func move(by: Coordinate) {
        aim += by.depth
        location = Coordinate(
            depth: location.depth + (by.position * aim),
            position: location.position + by.position)
    }
}

func parseInstruction(s: Substring) throws -> Coordinate {
    let splat = s.split(separator: " ")
    if splat.count != 2 {
        throw SimpleError.error("Invalid instruction: \(s))")
    }
    guard let count = Int(splat[1]) else {
        throw SimpleError.error("Distance is not an integer: \(splat[1])")
    }

    switch splat[0] {
    case "forward":
        return Coordinate(depth: 0, position: count)
    case "down":
        return Coordinate(depth: count, position: 0)
    case "up":
        return Coordinate(depth: -count, position: 0)
    case let x:
        throw SimpleError.error("Unknown direction: \(x)")
    }
}

func day02(_ args: ArraySlice<String>) throws {
    guard let input = args.first else {
        throw SimpleError.error("Usage: adventofcode 2 <input>")
    }
    let data = try parseLines(path: input, converter: parseInstruction)

    let part1 = data.reduce(Coordinate(), +)
    print("Part 1:")
    dumpResult(result: part1)

    let sub = Submarine()
    for inst in data {
        sub.move(by: inst)
    }
    print("Part 2")
    dumpResult(result: sub.location)
}

func dumpResult(result: Coordinate) {
    print(" Depth: \(result.depth)")
    print(" Position: \(result.position)")
    print(" Result: \(result.position * result.depth)")
}