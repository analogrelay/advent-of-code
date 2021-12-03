//
// Created by Andrew Stanton-Nurse on 12/3/21.
//

import Foundation

func parseLines<T>(path: String, converter: (Substring) throws -> T) throws -> [T] {
    let content = try String(contentsOfFile: path)
    let lines = content.split(separator: "\n")

    return try lines.map(converter)
}

func parseIntLines(path: String) throws -> [Int] {
    try parseLines(path: path, converter: { s in
        guard let i = Int(s) else {
            throw SimpleError.error("Failed to parse '\(s)' as an integer.")
        }
        return i
    })
}