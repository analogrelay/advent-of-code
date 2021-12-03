//
//  main.swift
//  adventofcode
//
//  Created by Andrew Stanton-Nurse on 12/2/21.
//
//

import Foundation

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

switch dayNumber {
case 1:
    day01(args);
    break;
default:
    fputs("Day \(dayNumber) not implemented!\n", stderr)
    exit(1)
}