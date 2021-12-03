//
//  main.swift
//  adventofcode
//
//  Created by Andrew Stanton-Nurse on 12/2/21.
//
//

import Foundation

func runDay(number: Int, args: ArraySlice<String>) throws {
    switch number {
    case 1:
        try day01(args);
        break;
    default:
        fputs("Day \(number) not implemented!\n", stderr)
        exit(1)
    }
}

if CommandLine.arguments.count < 2 {
    fputs("Usage: adventofcode <day> <args>\n", stderr)
    exit(1)
}

var day = CommandLine.arguments[1]
var args = CommandLine.arguments[2...]

guard let dayNumber = Int(day) else {
    fputs("Day '\(day)' is not a number.\n", stderr)
    exit(1)
}

do {
    try runDay(number: dayNumber, args: args)
} catch SimpleError.error(let s) {
    fputs("\(s)\n", stderr)
    exit(1)
} catch let e {
    fputs("Unhandled error: \(e)\n", stderr)
    exit(2)
}