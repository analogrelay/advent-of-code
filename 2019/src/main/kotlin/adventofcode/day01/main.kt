package adventofcode.day01

import java.io.File

fun main(args: Array<String>) {
    val inputFile = if (args.size < 1) {
        System.err.println("Input file not selected. Choosing 'input.txt' in the current directory.")
        "input.txt"
    } else {
        args[0]
    }

    solve(File(inputFile).readLines().map(::parseLine))
}

fun parseLine(input: String): Int {
    return input.toInt()
} 

fun solve(input: List<Int>) {
    val part1Result = input.map(::computeFuel).sum()
    println("[Part 1] Result: $part1Result");

    var part2Result = input.map(::computeAllFuel).sum()
    println("[Part 2] Result: $part2Result");
}

fun computeFuel(mass: Int) = Math.floor(mass.toDouble() / 3.0).toInt() - 2

fun computeAllFuel(currentMass: Int) = computeAllFuel(currentMass, 0)
tailrec fun computeAllFuel(currentMass: Int, fuelTotal: Int = 0): Int {
    val fuelMass = computeFuel(currentMass)
    if (fuelMass <= 0) {
        return fuelTotal
    } else {
        return computeAllFuel(fuelMass, fuelTotal + fuelMass)
    }
}
