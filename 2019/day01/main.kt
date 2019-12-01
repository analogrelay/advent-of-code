import kotlin.system.exitProcess

import java.io.File

fun main(args: Array<String>) {
    if (args.size < 1) {
        System.err.println("Missing required argument: input file")
        exitProcess(1)
    }

    val results = File(args[0]).readLines().map {
        val mass = Integer.parseInt(it, 10)
        val fuelRequired = computeFuel(mass)

        // Now calculate extra fuel required for that fuel
        var extraFuel = 0
        var remainingMass = fuelRequired
        while (remainingMass > 0) {
            extraFuel += remainingMass
            remainingMass = computeFuel(remainingMass)
        }
        Pair(fuelRequired, extraFuel)
    }

    val part1Result = results.map { it.first }.sum()
    println("[Part 1] Result: $part1Result");

    var part2Result = results.map { it.second }.sum()
    println("[Part 2] Result: $part2Result");
}

fun computeFuel(mass: Int): Int {
    return Math.floor(mass.toDouble() / 3.0).toInt() - 2
}
