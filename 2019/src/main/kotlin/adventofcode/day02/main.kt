package adventofcode.day02

import java.io.File

import adventofcode.common.IntcodeComputer

fun main(args: Array<String>) {
    var arguments = args.asList()
    val trace = arguments.contains("--trace")
    if (trace) {
        arguments = arguments.filterNot { it == "--trace" }
    }

    val enable1202 = arguments.contains("--enable-1202")
    if (enable1202) {
        arguments = arguments.filterNot { it == "--enable-1202" }
    }

    val inputFile = if (arguments.size < 1) {
        System.err.println("Usage: adventofcode day02 <INPUT FILE> [--trace] [--enable-1202]")
        System.exit(1)
        throw Exception("Whoop")
    } else {
        arguments[0]
    }

    val program = File(inputFile).readText().split(",").map(::parseItem).toIntArray()
    solvePart1(program, trace, enable1202)
    solvePart2(program, trace)
}

fun parseItem(input: String): Int = input.toInt()

fun solvePart1(program: IntArray, trace: Boolean, enable1202: Boolean) {
    var memory = program.copyOf();
    if (enable1202) {
        // Patch the 1202 noun and verb in
        memory[1] = 12
        memory[2] = 2
    }

    val computer = IntcodeComputer(memory, trace)
    computer.runToHalt()

    println("[Part 1] Result: ${computer.memory[0]}")
}

const val TARGET = 19690720

fun solvePart2(program: IntArray, trace: Boolean) {
    for (noun in 0..99) {
        for (verb in 0..99) {
            var memory = program.copyOf()

            // Patch memory
            memory[1] = noun
            memory[2] = verb

            // Run and test the output
            val computer = IntcodeComputer(memory, trace)
            computer.runToHalt()

            if (computer.memory[0] == TARGET) {
                val result = (100 * noun) + verb
                println("[Part 2] Result: $result")
                return
            }
        }
    }

    println("[Part 2] PANIC: No result found")
}