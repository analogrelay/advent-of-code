package adventofcode

import java.io.File;

fun main(args: Array<String>) {
    if (args.size < 1 || args[0] == "help") {
        println("Usage: adventofcode <dayNumber> <ARGS...>")
        System.exit(1)
    }

    val dayName = if (args[0].startsWith("day")) {
        args[0].substring(3)
    } else {
        args[0]
    }
    val dayNumber = dayName.toInt()

    println("Running solution for day $dayNumber");

    when (dayNumber) {
        1 -> adventofcode.day01.main(args.drop(1).toTypedArray())
        2 -> adventofcode.day02.main(args.drop(1).toTypedArray())
        3 -> adventofcode.day03.main(args.drop(1).toTypedArray())
        else -> throw Exception("Day not yet implemented: $dayNumber")
    }
}