package adventofcode

import java.io.File;

import adventofcode.common.IntcodeComputer

fun main(args: Array<String>) {
    if (args.size < 1 || args[0] == "help") {
        println("Usage: adventofcode <dayNumber> <ARGS...>")
        System.exit(1)
    }

    if (args[0] == "intcode") {
        println("Running Intcode computer")
        runIntcode(args.drop(1))
        return
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
        4 -> adventofcode.day04.main(args.drop(1).toTypedArray())
        5 -> adventofcode.day05.main(args.drop(1).toTypedArray())
        6 -> adventofcode.day06.main(args.drop(1).toTypedArray())
        else -> throw Exception("Day not yet implemented: $dayNumber")
    }
}

fun runIntcode(a: List<String>) {
    var args = a

    val trace = args.contains("--trace")
    if (trace) {
        args = args.filterNot { it == "--trace" }
    }

    if (args.size < 1) {
        System.err.println("Usage: adventofcode intcode <PROGRAM>")
        System.err.println("Usage: adventofcode intcode @<PROGRAMFILE>")
        System.exit(1)
    }

    val program = if (args[0].startsWith("@")) {
        File(args[0].substring(1)).readText().trim()
    } else {
        args[0]
    }

    val computer = IntcodeComputer.fromProgram(program, trace)
    computer.runToHalt()
}